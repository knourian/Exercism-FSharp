module Bandwagoner

type Coach = 
   {
      Name : string
      FormerPlayer : bool
   }

type Stats = 
   {
      Wins : int
      Losses : int
   }

type Team = 
   {
      Name : string
      Coach : Coach
      Stats : Stats
   }

let createCoach (name: string) (formerPlayer: bool): Coach =
   let coach:  Coach = {Name = name ; FormerPlayer = formerPlayer}
   coach

let createStats(wins: int) (losses: int): Stats =
   let stats : Stats = {Wins= wins;Losses = losses}
   stats

let createTeam(name: string) (coach: Coach)(stats: Stats): Team =
  let team : Team = {Name = name;Coach = coach ; Stats = stats}
  team 

let replaceCoach(team: Team) (coach: Coach): Team =
   let newTeamCoach: Team = { team with Coach = coach}
   newTeamCoach

let isSameTeam(homeTeam: Team) (awayTeam: Team): bool =
   homeTeam = awayTeam

let rootForTeam(team: Team): bool =
   match team with
   | {Coach = coach} when coach.Name.Equals("Gregg Popovich")  || coach.FormerPlayer = true -> true
   | {Name = name} when name.Equals("Chicago Bulls") -> true
   | {Stats = stats} when stats.Wins >= 60 || stats.Losses > stats.Wins -> true
   | _ -> false

