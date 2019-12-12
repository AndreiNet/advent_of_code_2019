extern crate num_bigint;
extern crate num_traits;

mod errors;

use std::str::FromStr;
use std::collections::HashMap;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

fn str_to_numbers(s: &str) -> Vec<BigInt> {
    s.split(',')
        .filter(|s| s.trim() != "")
        .map(|s| BigInt::from_str(s.trim()).unwrap())
        .collect()
}

fn get_input() -> Vec<BigInt> {
    let stdin = std::io::stdin();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    str_to_numbers(&line)
}

fn to_map(prog: Vec<BigInt>) -> HashMap<BigInt, BigInt> {
    let mut new_prog = HashMap::new();
    prog.into_iter().zip(0..).for_each(
        |(val, index)| { new_prog.insert(BigInt::from(index), val); }
    );
    new_prog
}

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

struct Input {
    numbers: Vec<BigInt>,
}

impl Input {
    fn single(val: BigInt) -> Input {
        Input {
            numbers: vec![val],
        }
    }

    fn empty() -> Input {
        Input {
            numbers: Vec::new(),
        }
    }

    fn from_slice(slice: &[BigInt]) -> Input {
        Input {
            numbers: slice.to_vec()
        }
    }

    fn pop(&mut self) -> Result<BigInt, String> {
        self.numbers.pop().ok_or_else(errors::no_more_input)
    }
}
struct State {
    env: HashMap<BigInt, BigInt>,
    pos: BigInt,
    relative_base: BigInt,
}

impl State {
    fn new(env: HashMap<BigInt, BigInt>) -> State {
        State {
            env,
            pos: BigInt::from(0),
            relative_base: BigInt::from(0),
        }
    }

    fn make_move(&mut self, input: &mut Input) -> Result<(Option<BigInt>, bool), String> {
        if &self.pos < &BigInt::from(0) {
            return Err(errors::no_instruction(&self.pos));
        }
        let &mut State{ ref mut env, ref mut pos, ref mut relative_base } = self;
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
                Ok((None, false))
            },
            3 => {
                let c = get_ref(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                *c = input.pop()?;
                *pos += 2;
                Ok((None, false))
            },
            4 => {
                let c = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                *pos += 2;
                Ok((Some(c), false))
            },
            5 => {
                let a = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                let b = get_value_all_modes(env, &((pos as &BigInt) + 2), relative_base, instr / 1000 % 10)?;
                if a != BigInt::from(0) {
                    *pos = b;
                } else {
                    *pos += 3;
                }
                Ok((None, false))
            },
            6 => {
                let a = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                let b = get_value_all_modes(env, &((pos as &BigInt) + 2), relative_base, instr / 1000 % 10)?;
                if a == BigInt::from(0) {
                    *pos = b;
                } else {
                    *pos += 3;
                }
                Ok((None, false))
            },
            9 => {
                let c = get_value_all_modes(env, &((pos as &BigInt) + 1), relative_base, instr / 100 % 10)?;
                *relative_base += c;
                *pos += 2;
                Ok((None, false))
            }
            99 => Ok((None, true)),
            _ => Err(errors::invalid_instruction(instr % 100)),
        }
    }

    fn run(&mut self, input: &mut Input) -> Vec<BigInt> {
        let mut output = Vec::new();
        let mut halt = false;
        while !halt {
            let (curr_output, halt_now) = self.make_move(input)
                .map_err(|err| panic!("{}", err + &errors::at_instruction(&self.pos))).unwrap();
            match curr_output {
                Some(val) => output.push(val),
                None => {},
            }
            halt = halt_now;
        }
        output
    }
}

fn task() {
    let prog = get_input();
    let env = to_map(prog);
    let mut state = State::new(env);
    let mut input = Input::single(BigInt::from(2));
    let output = state.run(&mut input);
    output.iter().for_each(|n| print!("{}", n));
    println!();
}

#[cfg(test)]
mod test {
    use super::{State, Input, to_map, str_to_numbers};
    #[test]
    fn test1() {
        let prog = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
        let env = to_map(str_to_numbers(prog));
        let mut state = State::new(env);
        let mut input = Input::empty();
        let output = state.run(&mut input);
        let expected_output = str_to_numbers(prog);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test2() {
        let prog = "1102,34915192,34915192,7,4,7,99,0";
        let env = to_map(str_to_numbers(prog));
        let mut state = State::new(env);
        let mut input = Input::empty();
        let output = state.run(&mut input);
        assert_eq!(output[0].to_string().len(), 16)
    }

    #[test]
    fn test3() {
        let prog = "104,1125899906842624,99";
        let env = to_map(str_to_numbers(prog));
        let mut state = State::new(env);
        let mut input = Input::empty();
        let output = state.run(&mut input);
        assert_eq!(&output[0].to_string(), "1125899906842624");
    }
}

fn main() {
    task();
}
