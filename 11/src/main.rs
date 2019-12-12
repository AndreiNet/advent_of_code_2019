extern crate num_bigint;
extern crate num_traits;

mod program;
mod robot;

use std::collections::HashMap;
use std::str::FromStr;
use std::sync::mpsc::channel;
use std::thread;
use num_bigint::BigInt;

use program::{State};
use robot::{Robot};

fn str_to_numbers(s: &str) -> Vec<BigInt> {
    s.split(',')
        .filter(|s| s.trim() != "")
        .map(|s| BigInt::from_str(s.trim()).unwrap())
        .collect()
}

fn to_map(prog: Vec<BigInt>) -> HashMap<BigInt, BigInt> {
    let mut new_prog = HashMap::new();
    prog.into_iter().zip(0..).for_each(
        |(val, index)| { new_prog.insert(BigInt::from(index), val); }
    );
    new_prog
}

fn get_input() -> HashMap<BigInt, BigInt> {
    let stdin = std::io::stdin();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    to_map(str_to_numbers(&line))
}
fn main() {
    let env = get_input();
    let (main_sender, main_receiver) = channel();
    let (second_sender, second_receiver) = channel();
    let (robot_output, program_input) = channel();
    let (program_output, robot_input) = channel();
    thread::spawn(move || {
        println!("[robot_thread]");
        let mut robot = Robot::new(robot_input, robot_output);
        if let Err(err) = robot.run() {
            panic!("{}", err);
        }
        main_sender.send(robot.number_painted()).expect("Main send error");
        second_sender.send(robot.painted()).expect("Second send error");
    });
    thread::spawn(move || {
        println!("[program_thread]");
        let mut program = State::new(env, program_input, program_output);
        program.run();
    });
    let ans = main_receiver.recv().unwrap();
    println!("{}", ans);
    let mat = second_receiver.recv().unwrap();
    for line in mat.into_iter() {
        println!("{}", String::from_utf8(line).unwrap());
    }
}
