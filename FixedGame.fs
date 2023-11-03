// For more information see https://aka.ms/fsharp-console-apps
type Details = {
    name: string
    description: string

}

type Item = {
    Details: Details
}

type RoomID = 
    |RoomID of string

type Exit =
    | PassableExit of string * destination: RoomID
    | LockedExit of string * key: Item * Exit
    | NoExit of string option

type Exits = {
    North: Exit
    East: Exit
    South: Exit
    West: Exit

}

type Room = {
    ID: RoomID
    Details: Details
    Items: Item list
    Exits: Exits
    
}

type Player = {
    Details: Details
    Location: RoomID
    Inventory: Item list
}

type World ={
    Rooms: Map<RoomID, Room>
    Player: Player
}


let key: Item = {
    Details ={
        name = "A shiny key"
        description = "Might open a door nearby"
    } 
}

let allRooms: Room list = [
    //center
    {
        ID = RoomID "Center"
        Details ={
            name = "A central room"
            description = "You are standing in a central room with exits in all directions. A single brazier lights the room"
        }
        Items = []
        Exits = {
            North = PassableExit ("You see a darkened passageway to the north.", RoomID "North")
            South = PassableExit("You see door to the south. A waft of cold air hits your face.", RoomID "south1")
            East = LockedExit ("You see a locked door to the east.", key, PassableExit ("You see an open door to the east.", RoomID "east1"))
            West = PassableExit ("You see an intreresting room to the west.", RoomID "west1")
            
        }

    }

//north1 room
    {
        ID = RoomID "North1"
        Details = {
            name = "A dark room"
            description = "You are standing in a very dark room. You hear the faint sound of rats scurry along the floor."
        }
        Items = []
        Exits = {
            North = NoExit None
            South = PassableExit("You see a dimly lit room to the south", RoomID "Center")
            East = NoExit None
            West = NoExit None
        }
    }

//south1 room
    {
        ID = RoomID "South1"
        Details = {
            name = "A cold room"
            description = "You are standing in room that feels very cold. Your breath instantly turns into a white puff."
        }
        Items = []
        Exits = {
            North = PassableExit ("You see an exit to the north. That room looks numch warmer.", RoomID "center")
            South = NoExit None
            East = NoExit None
            West = NoExit None
        }
    }


//west1 room
    {
        ID = RoomID "West1"
        Details = {
            name = "A cozy room"
            description = "This room seems very cozy, as if someone had made a home here. Various personal belongings are strewn about."
        }
        Items = [key]
        Exits = {
            North = NoExit None
            South = NoExit None
            East = PassableExit ("You see a doorway back to the lit room.", RoomID "center")
            West = NoExit None
        }
    }

//east1 room
    {
        ID = RoomID "East1"
        Details = {
            name = "An open meadow"
            description = "You are in an open meadow. The sun is bright and it takes some time for your eyes to adjust"
        }
        Items = []
        Exits = {
            North = NoExit None
            South = NoExit None
            East =  NoExit None
            West = PassableExit ("You see stone doorway to the west. Why would you want to go back there?", RoomID "center")
        }
    }
]

let player: Player = {
    Details = {
        name = "Ken"
        description = "Beach"
    }
    Inventory = []
    Location = RoomID "Center"

}


let gameWorld: World = {
    Rooms = 
        allRooms
        |> Seq.map (fun room -> (room.ID, room))
        |> Map.ofSeq
    Player = player

}


type Result<'TSuccess, 'TFailure> =
    |Success of 'TSuccess
    |Failure of 'TFailure



let extractDetailsFromRoom (room: Room) =
    room.Details




let switch processFunc input =
     Success(processFunc input)



let getExit direction exits =
    match (direction exits) with
    | PassableExit(_, roomId) -> Success roomId
    | LockedExit(_, _, _) -> Failure "There is a locked door in that direction."
    | NoExit(_) -> Failure "There is no room in that direction."



let setCurrentRoom world room ={ 
    world with Player = { 
            world.Player with Location = room.ID 
            } 
        }



let getRoom world roomID = 
    match world.Rooms.TryFind roomID with
    |Some room -> Success room
    |None -> Failure "Room does not exit"



let bind processFunc lastResult =
     match lastResult with
        | Success s -> processFunc s
        | Failure f -> Failure f 


let describeDetails details = 
    printfn "\n\n%s\n\n%s\n\n" details.name details.description



let describeCurrentRoom world =
    world.Player.Location
    |> getRoom world
    |> (bind (switch extractDetailsFromRoom) >> bind (switch describeDetails))


let north ({North = northExit}: Exits) = northExit
let south ({South = southExit}: Exits) = southExit
let east ({East = eastExit}: Exits) = eastExit
let west ({West = westExit}: Exits) = westExit

let getCurrentRoom world = 
    world.Player.Location
    |>getRoom world


let move direction world = 
    world
    |> getCurrentRoom
    |> (fun room -> room.exits)
    |> getExit direction


gameWorld
|> move south
|> bind (move north)
|> bind (switch  describeCurrentRoom)
|> ignore