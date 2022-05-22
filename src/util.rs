use enum_map::{Enum, EnumMap};
use std::io;
use std::io::Write;

#[derive(Enum, Debug, Clone, Copy)]
pub enum Resource {
    Lumber,
    Brick,
    Wool,
    Grain,
    Ore,
}

impl TryFrom<&str> for Resource {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "lumber" => Ok(Resource::Lumber),
            "brick" => Ok(Resource::Brick),
            "wool" => Ok(Resource::Wool),
            "grain" => Ok(Resource::Grain),
            "ore" => Ok(Resource::Ore),
            _ => Err(()),
        }
    }
}

pub const N_PLAYERS: usize = 4;
pub type Hand = EnumMap<Resource, u8>;
pub type State = [Hand; N_PLAYERS];

/// Finds the possible hands given the number of cards.
/// Stars and bars algorithm with fixed k=5
pub fn possible_hands(count: u8) -> Vec<Hand> {
    let mut result = Vec::new();
    let mut bins = [0; 5];
    bins[0] = count;
    loop {
        result.push(Hand::from_array(bins.clone()));
        if bins.last().unwrap() == &count {
            return result;
        }
        if bins[0] > 0 {
            bins[0] -= 1;
            bins[1] += 1;
        } else {
            let mut i = 1;
            while bins[i] == 0 {
                i += 1;
            }
            bins[0] = bins[i] - 1;
            bins[i + 1] += 1;
            bins[i] = 0;
        }
    }
}

/// Gets input string from user
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

/// https://gist.github.com/Prakasaka/219fe5695beeb4d6311583e79933a009
pub fn format_str<T: AsRef<str>>(text: T, color: &str) -> String {
    match color {
        "black" => format!("\x1b[30m{}\x1b[0m", text.as_ref()),
        "red" => format!("\x1b[31m{}\x1b[0m", text.as_ref()),
        "green" => format!("\x1b[32m{}\x1b[0m", text.as_ref()),
        "yellow" => format!("\x1b[33m{}\x1b[0m", text.as_ref()),
        "blue" => format!("\x1b[34m{}\x1b[0m", text.as_ref()),
        "purple" => format!("\x1b[35m{}\x1b[0m", text.as_ref()),
        "cyan" => format!("\x1b[36m{}\x1b[0m", text.as_ref()),
        "white" => format!("\x1b[37m{}\x1b[0m", text.as_ref()),
        _ => text.as_ref().to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{possible_hands, Hand, Resource};

    #[test]
    fn test_enum_map() {
        let mut z = Hand::default();
        z[Resource::Lumber] += 1;
        z[Resource::Wool] += 3;

        assert_eq!(z[Resource::Lumber], 1);
        assert_eq!(z[Resource::Wool], 3);
    }

    #[test]
    fn test_possible_hands() {
        let n = 0;
        let hands = possible_hands(n);
        assert_eq!(1, hands.len());

        let n = 5;
        let hands = possible_hands(n);
        assert_eq!(126, hands.len());

        let n = 19; // most cards of one type a player could have
        let hands = possible_hands(n);
        assert_eq!(8855, hands.len());
    }

    #[test]
    fn test_color() {
        use super::format_str;
        println!("{}", format_str("Hello", "red"));
        println!("{}", format_str("Hello", "blue"));
        println!("{}", format_str("Hello", "orange"));
        println!("{}", format_str("Hello", "green"));
        println!("{}", format_str("Hello", "yellow"));
        println!("{}", format_str("Hello", "none"));
    }
}
