use std::{env::args, fmt::Display, mem::transmute, process::exit};

use getrandom::getrandom;
use owo_colors::OwoColorize;

#[derive(PartialEq)]
enum Advness {
    Normal,
    Adv,
    Dis,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy)]
struct Dice {
    count: i16,
    value: u16,
}

#[derive(Clone, Copy)]
enum Critness {
    Constant,
    None,
    Hit,
    Miss,
}

impl Critness {
    fn print(self, text: impl Display + OwoColorize) {
        match self {
            Self::Constant => print!("{}", text.default_color()),
            Self::None => print!("{}", text.bold()),
            Self::Hit => print!("{}", text.bold().green()),
            Self::Miss => print!("{}", text.bold().red()),
        }
    }
}

#[derive(Clone, Copy)]
struct Roll {
    roll: u16,
    critness: Critness,
}

impl Roll {
    fn print(self) {
        self.critness.print(self.roll);
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Sign {
    Pos,
    Neg,
}

impl From<i16> for Sign {
    fn from(x: i16) -> Self {
        match x.is_negative() {
            true => Sign::Neg,
            false => Sign::Pos,
        }
    }
}

impl Sign {
    fn signum(self) -> i16 {
        match self {
            Self::Pos => 1,
            Self::Neg => -1,
        }
    }
}

#[derive(Clone, Copy)]
struct ReducedRoll {
    sign: Sign,
    roll: Roll,
}

impl ReducedRoll {
    fn print(self) {
        print!(
            "{} ",
            match self.sign {
                Sign::Pos => '+',
                Sign::Neg => '-',
            }
        );

        self.roll.print();
    }

    fn print_elided(self) {
        if self.sign == Sign::Neg {
            print!("- ");
        }

        self.roll.print();
    }
}

struct Rolls {
    sign: Sign,
    rolls: Vec<Roll>,
}

impl Rolls {
    fn print_internal(&self) {
        if let Some(roll) = self.rolls.first() {
            roll.print();
        }

        for roll in &self.rolls[1..] {
            print!(" ");
            roll.print();
        }
    }

    fn print(&self) {
        print!(
            "{}",
            match self.sign {
                Sign::Pos => '+',
                Sign::Neg => '-',
            }
        );

        if !self.rolls.is_empty() {
            print!(" ");
        }

        self.print_internal();
    }

    fn print_elided(&self) {
        if self.sign == Sign::Neg {
            print!("-");

            if !self.rolls.is_empty() {
                print!(" ");
            }
        }

        self.print_internal();
    }

    fn reduce(self) -> ReducedRoll {
        ReducedRoll {
            sign: self.sign,
            roll: self.rolls.into_iter().fold(
                Roll {
                    roll: 0,
                    critness: Critness::Constant,
                },
                |acc, roll| Roll {
                    roll: acc.roll + roll.roll,
                    critness: match (acc.critness, roll.critness) {
                        (Critness::Hit, Critness::Hit)
                        | (Critness::Hit, Critness::Constant)
                        | (Critness::Constant, Critness::Hit) => Critness::Hit,
                        (Critness::Miss, Critness::Miss)
                        | (Critness::Miss, Critness::Constant)
                        | (Critness::Constant, Critness::Miss) => Critness::Miss,
                        (Critness::Constant, Critness::Constant) => Critness::Constant,
                        _ => Critness::None,
                    },
                },
            ),
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy)]
enum Arg {
    Dice(Dice),
    Literal(i16),
}

impl Arg {
    fn dice(count: i16, value: u16) -> Self {
        Self::Dice(Dice { count, value })
    }

    fn roll(self) -> Rolls {
        match self {
            Self::Dice(die) => {
                let mut buf = [0; 16];

                if die.value == 0 {
                    eprintln!("cannot roll a d0");
                    exit(1);
                }

                let sign = die.count.into();

                Rolls {
                    sign,
                    rolls: (0..die.count.abs())
                        .map(|_| {
                            getrandom(&mut buf).unwrap();

                            let roll = (unsafe { transmute::<_, u128>(buf) } % die.value as u128)
                                as u16
                                + 1;
                            Roll {
                                roll,
                                critness: match (roll, sign) {
                                    _ if die.value == 1 => Critness::Constant,
                                    (roll, Sign::Pos) if roll == die.value => Critness::Hit,
                                    (roll, Sign::Neg) if roll == die.value => Critness::Miss,
                                    (1, Sign::Pos) => Critness::Miss,
                                    (1, Sign::Neg) => Critness::Hit,
                                    _ => Critness::None,
                                },
                            }
                        })
                        .collect(),
                }
            }
            Self::Literal(literal) => Rolls {
                sign: literal.into(),
                rolls: vec![Roll {
                    roll: literal.unsigned_abs(),
                    critness: Critness::Constant,
                }],
            },
        }
    }
}

fn main() {
    let mut args = args().skip(1);
    let mut tokens = match args.next() {
        Some(tokens) => tokens.chars().collect::<Vec<_>>(),
        None => Vec::default(),
    };

    if let Some(arg) = args.next() {
        eprintln!("unexpected arg: {arg}; expected up to one argument");
        exit(1);
    }

    let advness = match tokens[..] {
        [.., 'a'] => Advness::Adv,
        [.., 'd'] => Advness::Dis,
        _ => Advness::Normal,
    };

    if let Advness::Adv | Advness::Dis = advness {
        tokens.pop();
    }

    let mut args = Vec::default();
    let mut first = true;
    let mut numeral = Vec::default();
    let mut sign = None;
    let mut count = None;

    for token in tokens {
        match token {
            '0'..='9' => numeral.push(token),
            '-' | '+' if sign.is_none() => {
                if first {
                    args.push(Arg::dice(1, 20));
                    first = false;
                }

                if let Some(count_val) = count {
                    let Ok(value) = 
                        numeral
                            .into_iter()
                            .collect::<String>()
                            .parse()
                            else {
                                eprintln!("expected dice sides for {count_val}");
                                exit(1);
                            };
                    numeral = Vec::default();

                    args.push(Arg::dice(count_val, value));

                    count = None;
                } else if !numeral.is_empty() {
                    if sign == Some('-') {
                        numeral.insert(0, '-');
                    }

                    args.push(Arg::Literal(
                        numeral.into_iter().collect::<String>().parse().unwrap(),
                    ));
                    numeral = Vec::default();
                }

                sign = Some(token);
            }
            'd' => {
                if numeral.is_empty() {
                    numeral.push('1');
                }

                if sign == Some('-') {
                    numeral.insert(0, '-');
                }
                sign = None;

                count = Some(numeral.into_iter().collect::<String>().parse().unwrap());
                numeral = Vec::default();

                first = false;
            }
            token => {
                eprintln!("unexpected token: {token}");
                exit(1);
            }
        }
    }

    if first {
        args.push(Arg::dice(1, 20));
    }

    if let Some(count) = count {
        let Ok(value) = 
            numeral
                .into_iter()
                .collect::<String>()
                .parse()
                 else {
                    eprintln!("expected dice sides for {count}d");
                    exit(1);
                };

        args.push(Arg::dice(count, value));
    } else if !numeral.is_empty() {
        if sign == Some('-') {
            numeral.insert(0, '-');
        }

        args.push(Arg::Literal(
            numeral.into_iter().collect::<String>().parse().unwrap(),
        ));
    } else if let Some(sign) = sign {
        args.push(Arg::Literal(match sign {
            '+' => 1,
            '-' => -1,
            _ => unreachable!(),
        }));
    }

    let rolls = (0..match advness {
        Advness::Normal => 1,
        Advness::Adv | Advness::Dis => 2,
    })
        .map(|_| {
            let rolls = args.iter().map(|arg| arg.roll()).collect::<Vec<_>>();

            if rolls.iter().any(|rolls| rolls.rolls.len() > 1) {
                if let Some(rolls) = rolls.first() {
                    rolls.print_elided();
                }
                for rolls in &rolls[1..] {
                    print!(" ");
                    rolls.print();
                }
                println!();
            }

            let rolls = rolls
                .into_iter()
                .map(|rolls| rolls.reduce())
                .collect::<Vec<_>>();

            if rolls.len() > 1 {
                rolls[0].print_elided();

                for rolls in &rolls[1..] {
                    print!(" ");
                    rolls.print();
                }
                println!();
            }

            let (roll, critness) =
                rolls
                    .into_iter()
                    .fold((0, Critness::Constant), |(sum, critness), roll| {
                        (
                            sum + roll.sign.signum() * roll.roll.roll as i16,
                            match (critness, roll.roll.critness) {
                                (Critness::Hit, Critness::Hit)
                                | (Critness::Hit, Critness::Constant)
                                | (Critness::Constant, Critness::Hit) => Critness::Hit,
                                (Critness::Miss, Critness::Miss)
                                | (Critness::Miss, Critness::Constant)
                                | (Critness::Constant, Critness::Miss) => Critness::Miss,
                                (Critness::Constant, Critness::Constant) => Critness::Constant,
                                _ => Critness::None,
                            },
                        )
                    });

            let roll_str = format!("{roll}");

            critness.print(roll_str);
            println!();

            (roll, critness)
        });

    if let Advness::Adv | Advness::Dis = advness {
        let (roll, critness) = rolls
            .max_by(|(left, _), (right, _)| match advness {
                Advness::Adv => left.cmp(right),
                Advness::Dis => right.cmp(left),
                _ => unreachable!(),
            })
            .unwrap();
        let roll = format!("{roll}");

        critness.print(roll);
        println!();
    } else {
        rolls.for_each(|_| ());
    }
}
