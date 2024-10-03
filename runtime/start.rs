use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    match errcode {
        0 => eprintln!("an error ocurred: numeric operation resulted in overflow"),
        _ => ()
    }
    std::process::exit(1);
}

#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(value: i64, type_flag: i64) {
    fn inner_print(value: i64, type_kind: i64) -> i64{
        if type_kind == 0 {
            println!("{}", value);
        } else {
            if value == 0 {
                println!("false")
            } else {
                println!("true")
            }
        }
            return value
    }
    inner_print(value,type_flag);
}


fn parse_input(input: &str) -> i64 {
    // ADDED parse input number
    // CHANGE: printing the input was causing error so removed it
    let input_val_int: i64 = input.parse().expect("Input is not a valid i64");
    input_val_int
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "0" };
    let input = parse_input(&input);

    let _i: i64 = unsafe { our_code_starts_here(input) };
    // println!("{i}");
}

