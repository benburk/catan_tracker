use ndarray::{Array, Dim};

pub type Player = String;

// a hand is a 5-tuple of cards
pub type Cards = [u32; 5];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Resource {
    Lumber,
    Brick,
    Wool,
    Grain,
    Ore,
}

impl TryFrom<&str> for Resource {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "lumber" => Ok(Resource::Lumber),
            "brick" => Ok(Resource::Brick),
            "wool" => Ok(Resource::Wool),
            "grain" => Ok(Resource::Grain),
            "ore" => Ok(Resource::Ore),
            _ => Err(()),
        }
    }
}

pub enum Item {
    Road,
    Settlement,
    City,
    DevelopmentCard,
}

impl Item {
    fn cost(&self) -> Cards {
        match self {
            Item::Road => [1, 1, 0, 0, 0],
            Item::Settlement => [1, 1, 1, 1, 0],
            Item::City => [0, 0, 0, 2, 3],
            Item::DevelopmentCard => [0, 0, 1, 1, 1],
        }
    }
}

impl TryFrom<&str> for Item {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "road" => Ok(Item::Road),
            "settlement" => Ok(Item::Settlement),
            "city" => Ok(Item::City),
            "development card" => Ok(Item::DevelopmentCard),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    Receive(Player, Cards),
    Discard(Player, Cards),
    Spend(Player, Cards),
    Rob(Player, Player, Resource),
    Offer(Player, Cards, Cards),
    Trade(Player, Player, Cards, Cards),
    YearOfPlenty(Player, Cards),
    BankTrade(Player, Cards, Cards),
    Monopoly(Player, Resource),
}
