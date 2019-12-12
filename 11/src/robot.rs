
use std::sync::mpsc::{Receiver, Sender};
use std::collections::{HashMap, HashSet};

use num_bigint::BigInt;
pub struct Robot {
    input: Receiver<Option<BigInt>>,
    output: Sender<BigInt>,
    x: i32,
    y: i32,
    dir: i32,
    state: HashMap<(i32, i32), i32>,
    painted: HashSet<(i32, i32)>,
}

impl Robot {
    pub fn new(input: Receiver<Option<BigInt>>, output: Sender<BigInt>) -> Self {
        Robot {
            input,
            output,
            x: 0,
            y: 0,
            dir: 0,
            state: HashMap::new(),
            painted: HashSet::new(),
        }
    }

    pub fn make_move(&mut self) -> Result<bool, String> {
        let &mut Robot{ ref mut input, ref mut output, ref mut x,
                        ref mut y, ref mut dir, ref mut state, ref mut painted } = self;
        let curr_val = state.entry((*x, *y)).or_insert(0);
        output.send(BigInt::from(*curr_val)).expect("Robot send error");
        let new_color = match input.recv().expect("Robot recv error") {
            Some(val) => val,
            None => return Ok(true),
        };
        let turn = match input.recv().expect("Robot recv error") {
            Some(val) => val,
            None => return Ok(true),
        };
        let new_color = if new_color == BigInt::from(0) { 0 } else { 1 };
        let turn = if turn == BigInt::from(0) { 0 } else { 1 };
        *curr_val = new_color;
        *dir = if turn == 0 { (*dir - 1 + 4) % 4 } else { (*dir + 1) % 4 };
        painted.insert((*x, *y));
        let (dx, dy) = dir_to_move(*dir);
        *x += dx;
        *y += dy;
        Ok(false)
    }

    pub fn run(&mut self) -> Result<(), String> {
        // Comment / uncomment this line for task 2/1
        self.state.insert((0, 0), 1);
        while !self.make_move()? {}
        Ok(())
    }

    pub fn number_painted(&self) -> usize {
        self.painted.len()
    }

    pub fn painted(&self) -> Vec<Vec<u8>> {
        let maxx = self.painted.iter().map(|(x, _y)| x).max().unwrap() + 1;
        let minx = self.painted.iter().map(|(x, _y)| x).min().unwrap();
        let maxy = self.painted.iter().map(|(_x, y)| y).max().unwrap() + 1;
        let miny = self.painted.iter().map(|(_x, y)| y).min().unwrap();
        let mut mat = vec![vec![b'0'; (maxy - miny) as usize]; (maxx - minx) as usize];
        for (&(x, y), &val) in self.state.iter() {
            mat[(x - minx) as usize][(y - miny) as usize] = (b'0' + val as u8) as u8;
        }
        mat
    }
}

fn dir_to_move(dir: i32) -> (i32, i32) {
    match dir {
        0 => (-1, 0),
        1 => (0, 1),
        2 => (1, 0),
        3 => (0, -1),
        _ => panic!("Undefined direction"),
    }
}