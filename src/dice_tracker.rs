
pub struct DiceTracker {
    rolls: Vec<u8>,
}

impl DiceTracker {
    pub fn new() -> Self {
        Self {
            rolls: Vec::new(),
        }
    }

    pub fn push(&mut self, roll: u8) {
        self.rolls.push(roll);
    }
}