use std::fmt::Display;

pub(crate) fn cant_reach() -> ! {
    panic!("Should not reach here")
}

pub(crate) fn incorrect_mode<S: Display, T: Display>(pos: S, mode: T) -> String {
    format!("Incorrect mode for position {} and mode {}\n", pos, mode)
}

pub(crate) fn invalid_mode<S: Display>(mode: S) -> String {
    format!("Invalid mode {}\n", mode)
}

pub(crate) fn invalid_instruction<S: Display>(instr: S) -> String {
    format!("Invalid instruction {}\n", instr)
}

pub(crate) fn no_instruction<S: Display>(s: S) -> String {
    format!("No instruction at position {}\n", s.to_string())
}

pub(crate) fn no_value<S: Display>(pos: S) -> String {
    format!("No value at pos {}\n", pos)
}

pub(crate) fn at_instruction<S: Display>(pos: S) -> String {
    format!("At instruction {}\n", pos)
}
