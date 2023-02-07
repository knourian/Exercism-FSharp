module ValentinesDay

type Approval = 
    | Yes
    | No
    | Maybe

type Cuisine = 
    | Korean
    | Turkish

type Genre = 
    | Crime
    | Horror
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity): Approval = 
    match activity with
    | Activity.BoardGame -> Approval.No
    | Activity.Chill -> Approval.No
    | Activity.Movie Genre.Romance -> Approval.Yes
    | Activity.Movie _ ->Approval.No
    | Activity.Restaurant Cuisine.Korean -> Approval.Yes
    | Activity.Restaurant Cuisine.Turkish -> Approval.Maybe
    | Activity.Walk i when i<3 -> Approval.Yes
    | Activity.Walk i when i <5 && i>=3 -> Approval.Maybe
    | Activity.Walk _ -> Approval.No