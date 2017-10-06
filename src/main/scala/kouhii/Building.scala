package kouhii

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/**
  * Building object that generates rooms and item locations
  */
class Building {
  private val masterList = Random.shuffle(List[String](
    "kitchen", "bathroom", "powder room", "living room", "library", "bedroom",
    "bedroom", "master bedroom", "guest bedroom", "game room", "music study",
    "home theater", "bar", "indoor pool"
  ))
  private val building = Array.ofDim[Room](4, 4)
  private val buildingMap = Array.ofDim[Boolean](4, 4)
  
  // Edges of maps; the garage and foyer will be located in one of these tuples
  private val edgeRooms = Random.shuffle(List(
    (0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 3),
    (2, 0), (2, 3), (3, 0), (3, 1), (3, 2), (3, 3)
  ))
  private val foyerPos = edgeRooms.head
  private val garagePos = edgeRooms.last
  private var location = edgeRooms.head
  private var roomNum = 0
  private var numItems: Int = 0

  buildRoom(foyerPos, null)
  insertItems()

  /**
    * Builds a room in the building
    *
    * @param currentPos     A tuple of the current position in the map
    * @param previousPos    A tuple of the most recent position in the map
    */
  private def buildRoom(currentPos: (Int, Int), previousPos: (Int, Int)) {
    // Create the room and mark as visited
    val room: Room = {
      if (currentPos == foyerPos) {
        new Foyer
      }
      else if (currentPos == garagePos) {
        new Garage
      }
      else {
        masterList(roomNum) match {
          case "kitchen" => new Kitchen
          case "bathroom" => new Bathroom
          case "powder room" => new PowderRoom
          case "living room" => new LivingRoom
          case "library" => new Library
          case "bedroom" => new Bedroom
          case "master bedroom" => new MasterBedroom
          case "guest bedroom" => new GuestBedroom
          case "game room" => new GameRoom
          case "music study" => new MusicStudy
          case "home theater" => new HomeTheater
          case "bar" => new Bar
          case "indoor pool" => new Pool
          case "garage" => new Garage
          case _ => new Foyer
        }
      }
    }

    if (currentPos != foyerPos && currentPos != garagePos) {
      roomNum += 1
    }

    building(currentPos._1)(currentPos._2) = room
    buildingMap(currentPos._1)(currentPos._2) = true

    // Open a door to the previous room
    if (previousPos != null) {
      (currentPos._1 - previousPos._1, currentPos._2 - previousPos._2) match {
        case (-1, 0) =>
          building(currentPos._1)(currentPos._2).openDoor('S')
          building(previousPos._1)(previousPos._2).openDoor('N')
        case (0, 1) =>
          building(currentPos._1)(currentPos._2).openDoor('W')
          building(previousPos._1)(previousPos._2).openDoor('E')
        case (1, 0) =>
          building(currentPos._1)(currentPos._2).openDoor('N')
          building(previousPos._1)(previousPos._2).openDoor('S')
        case (0, -1) =>
          building(currentPos._1)(currentPos._2).openDoor('E')
          building(previousPos._1)(previousPos._2).openDoor('W')
      }
    }

    // Get a list of neighbors and iterate
    val neighBuffer = new ArrayBuffer[(Int, Int)]()

    if (currentPos._1 != 0) {
      neighBuffer += ((currentPos._1 - 1, currentPos._2))
    }
    if (currentPos._1 != 3) {
      neighBuffer += ((currentPos._1 + 1, currentPos._2))
    }
    if (currentPos._2 != 0) {
      neighBuffer += ((currentPos._1, currentPos._2 - 1))
    }
    if (currentPos._2 != 3) {
      neighBuffer += ((currentPos._1, currentPos._2 + 1))
    }

    val neighbors = Random.shuffle(neighBuffer).toList

    for (neighbor <- neighbors) {
      if (!buildingMap(neighbor._1)(neighbor._2)) {
        buildRoom(neighbor, currentPos)
      }
    }
  }

  /**
    * Insert the game items into various rooms
    */
  private def insertItems() {
    val itemList = Random.shuffle(List(
      "coffee", "cream", "sugar", "machine", "filter", "grinder", "mug", "spoon"
    ))
    
    // Create a list of the map coordinates and shuffle them
    val coordinates = Random.shuffle({
      val coordBuffer = new ArrayBuffer[(Int, Int)]

      for (i <- 0 until 4) {
        for (j <- 0 until 4) {
          coordBuffer += ((i, j))
        }
      }

      coordBuffer.toList
    })
    numItems = itemList.length

    // Insert the items into the shuffled coordinates
    for (i <- itemList.indices) {
      val item = itemList(i)
      val xy = coordinates(i)

      item match {
        case "coffee" => building(xy._1)(xy._2).setItem(new Coffee)
        case "cream" => building(xy._1)(xy._2).setItem(new Cream)
        case "sugar" => building(xy._1)(xy._2).setItem(new Sugar)
        case "machine" => building(xy._1)(xy._2).setItem(new CoffeeMachine)
        case "filter" => building(xy._1)(xy._2).setItem(new CoffeeFilter)
        case "grinder" => building(xy._1)(xy._2).setItem(new CoffeeGrinder)
        case "mug" => building(xy._1)(xy._2).setItem(new Mug)
        case _ => building(xy._1)(xy._2).setItem(new Spoon)
      }
    }
  }

  /**
    * Views the map of the building
    *
    * @return   ASCII art of the building's map
    */
  def viewMap: String = {
    val map = new StringBuilder(" ")
    map ++= "_" * 7
    map ++= "\n|"

    for (i <- building.indices) {
      for (j <- building(i).indices) {
        if ((i, j) == location) {
          map ++= "\033[41m"
        }

        if (building(i)(j).hasSouthDoor) {
          map ++= " "
        }
        else {
          map ++= "_"
        }

        if ((i, j) == location) {
          map ++= "\033[0m"
        }

        if (building(i)(j).hasEastDoor) {
          if (i == 3) {
            map ++= "_"
          }
          else {
            map ++= "."
          }
        }
        else {
          map ++= "|"
        }
      }
      if (i < 3) {
        map ++= "\n|"
      }
    }

    map.toString()
  }

  /**
    * Gets the foyer position in the map
    *
    * @return   A tuple of the foyer's coordinates
    */
  def getEntrance: (Int, Int) = {
    foyerPos
  }

  /**
    * Gets the player's current location
    *
    * @return   A tuple of the player's coordinates
    */
  def getLocation: (Int, Int) = {
    location
  }

  /**
    * Attempt to move the player north on the map
    *
    * @return   True if moved, false if stayed put
    */
  def moveNorth: Boolean = {
    if (location._1 == 0) {
      false
    }
    else {
      location = (location._1 - 1, location._2)
      true
    }
  }

  /**
    * Attempt to move the player east on the map
    *
    * @return   True if moved, false if stayed put
    */
  def moveEast: Boolean = {
    if (location._2 == 3) {
      false
    }
    else {
      location = (location._1, location._2 + 1)
      true
    }
  }

  /**
    * Attempt to move the player south on the map
    *
    * @return   True if moved, false if stayed put
    */
  def moveSouth: Boolean = {
    if (location._1 == 3) {
      false
    }
    else {
      location = (location._1 + 1, location._2)
      true
    }
  }

  /**
    * Attempt to move the player west on the map
    *
    * @return   True if moved, false if stayed put
    */
  def moveWest: Boolean = {
    if (location._2 == 0) {
      false
    }
    else {
      location = (location._1, location._2 - 1)
      true
    }
  }

  /**
    * Gets the Room object of the current location
    *
    * @return   The instance of the current coordinate's Room object
    */
  def getCurrentRoom: Room = {
    building(location._1)(location._2)
  }

  /**
    * Gets the number of items in the game
    *
    * @return   The number of items as an int
    */
  def getNumItems: Int = {
    numItems
  }
}
