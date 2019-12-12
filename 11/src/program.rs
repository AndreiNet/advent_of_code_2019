mod errors;

use std::collections::HashMap;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::sync::mpsc::{Receiver, Sender};

fn get_value_at_pos<'a>(env: &'a HashMap<BigInt, BigInt>, pos: &BigInt)
    -> Result<BigInt, String> {
    if pos < &BigInt::from(0) {
        return Err(errors::no_value(pos));
    }
    match env.get(pos) {
        Some(v) => Ok(v.clone()),
        None => Ok(BigInt::from(0)),
    }
}

fn get_ref_at_pos<'a>(env: &'a mut HashMap<BigInt, BigInt>, pos: &BigInt)
    -> Result<&'a mut BigInt, String> {
    if pos < &BigInt::from(0) {
        return Err(errors::no_value(pos));
    }
    Ok(env.entry(pos.clone()).or_insert(BigInt::from(0)))
}

fn get_value_all_modes<'a>(env: &'a HashMap<BigInt, BigInt>,
                           pos: &BigInt,
                           relative_base: &BigInt,
                           mode: i32) -> Result<BigInt, String> {
    match mode {
        1 => get_value_at_pos(env, pos),
        0 | 2 => {
            let new_pos = get_value_at_pos(env, pos)?;
            let real_pos = match mode {
                0 => new_pos.clone(),
                2 => new_pos + relative_base,
                _ => errors::cant_reach(),
            };
            get_value_at_pos(env, &real_pos)
        },
        _ => Err(errors::invalid_mode(mode)),
    }
}

fn get_ref<'a>(env: &'a mut HashMap<BigInt, BigInt>,
               pos: &BigInt,
               relative_base: &BigInt,
               mode: i32) -> Result<&'a mut BigInt, String> {
    match mode {
        1 => Err(errors::incorrect_mode(pos, mode)),
        0 | 2 =>  {
            let new_pos = get_value_at_pos(env, &pos)?;
            let real_pos = match mode {
                0 => new_pos.clone(),
                2 => new_pos + relative_base,
                _ => errors::cant_reach(),
            };
            get_ref_at_pos(env, &real_pos)
        },
        _ => Err(errors::invalid_mode(mode)),
    }
}

pub struct State {
    env: HashMap<BigInt, BigInt>,
    pos: BigInt,
    relative_base: BigInt,
    input: Receiver<BigInt>,
    output: Sender<Option<BigInt>>,
}

impl State {
    pub fn new(env: HashMap<BigInt, BigInt>,
               input: Receiver<BigInt>,
               output: Sender<Option<BigInt>>) -> State {
        State {
            env,
            pos: BigInt::from(0),
            relative_base: BigInt::from(0),
            input,
            output,
        }
    }

    pub fn make_move(&mut self) -> Result<bool, String> {
        if &self.pos < &BigInt::from(0) {
            return Err(errors::no_instruction(&self.pos));
        }
        let &mut State{ ref mut env, ref mut pos, ref mut relative_base, ref mut input, ref mut output } = self;
        let instr = get_value_at_pos(env, pos)
            .map_err(|err| err + &errors::at_instruction(&pos))?;
        let instr = (instr % 1000000i32).to_i32().unwrap();
        match instr % 100 {
            1 | 2 | 7 | 8 => {
                let a = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                let b = get_value_all_modes(env, &((pos as &BigInt) + 2), relative_base, instr / 1000 % 10)?;
                let c = get_ref(env, &((pos as &BigInt) + 3), relative_base, instr / 10000 % 10)?;
                match instr % 100 {
                    1 => *c = a + b,
                    2 => *c = a * b,
                    7 => *c = BigInt::from(if a < b { 1 } else { 0 }),
                    8 => *c = BigInt::from(if a == b { 1 } else { 0 }),
                    _ => errors::cant_reach(),
                };
                *pos += 4;
                Ok(false)
            },
            3 => {
                let c = get_ref(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                *c = input.recv().expect("Recv error");
                *pos += 2;
                Ok(false)
            },
            4 => {
                let c = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                output.send(Some(c)).expect("Send error");
                *pos += 2;
                Ok(false)
            },
            5 => {
                let a = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                let b = get_value_all_modes(env, &((pos as &BigInt) + 2), relative_base, instr / 1000 % 10)?;
                if a != BigInt::from(0) {
                    *pos = b;
                } else {
                    *pos += 3;
                }
                Ok(false)
            },
            6 => {
                let a = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                let b = get_value_all_modes(env, &((pos as &BigInt) + 2), relative_base, instr / 1000 % 10)?;
                if a == BigInt::from(0) {
                    *pos = b;
                } else {
                    *pos += 3;
                }
                Ok(false)
            },
            9 => {
                let c = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                *relative_base += c;
                *pos += 2;
                Ok(false)
            }
            99 => {
                output.send(None).expect("Send error");
                Ok(true)
            },
            _ => Err(errors::invalid_instruction(instr % 100)),
        }
    }

    pub fn run(&mut self) {
        let mut halt = false;
        while !halt {
            // println!("Program move");
            let halt_now = self.make_move()
                .map_err(|err| panic!("{}", err + &errors::at_instruction(&self.pos))).unwrap();
            halt = halt_now;
        }
    }
}