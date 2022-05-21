use enum_map::{enum_map, Enum, EnumMap};
use std::io;
use std::io::Write;
const N_PLAYERS: usize = 4;

#[derive(Enum, Debug)]
enum Resource {
    Lumber,
    Brick,
    Wool,
    Grain,
    Ore,
}

pub type Hand = EnumMap<Resource, u8>;
pub type State = [Hand; N_PLAYERS];

#[cfg(test)]
mod tests {
    use super::{Hand, Resource};
    use enum_map::{enum_map, EnumMap};

    #[test]
    fn test_enum_map() {
        let mut z = Hand::default();
        z[Resource::Lumber] += 1;
        z[Resource::Wool] += 3;

        assert_eq!(z[Resource::Lumber], 1);
        assert_eq!(z[Resource::Wool], 3);
    }
}

pub fn input(msg: &str) -> String {
    let mut stdout = io::stdout().lock();
    stdout.write_all(msg.as_bytes()).unwrap();
    stdout.flush().expect("error: unable to flush stdout");
    let mut buf = String::new();
    io::stdin()
        .read_line(&mut buf)
        .expect("error: unable to read user input");

    if buf.ends_with('\n') {
        buf.pop();
        if buf.ends_with('\r') {
            buf.pop();
        }
    }
    buf
}
