package kouhii

/**
  * Base trait used to define rooms in the game
  */
trait Room {
  protected var roomType: String
  protected var description: String
  protected var item: Item = null
  protected var northDoor: Boolean = false
  protected var eastDoor: Boolean = false
  protected var southDoor: Boolean = false
  protected var westDoor: Boolean = false

  /**
    * Gets the type of the room
    *
    * @return   A string of the room's type
    */
  def isType: String = {
    roomType
  }

  /**
    * Gets a description of the room
    *
    * @return   A string describing the room
    */
  def describe: String = {
    description
  }

  /**
    * Checks if the room has an item
    *
    * @return   True if the room has an item, false otherwise
    */
  def hasItem: Boolean = {
    item != null
  }

  /**
    * Takes an item from the room
    *
    * @return   The item as an Item class instance
    */
  def takeItem: Item = {
    val taken = item
    item = null
    taken
  }

  /**
    * Check if the room has a north door
    *
    * @return   True if the door exists, false otherwise
    */
  def hasNorthDoor: Boolean = {
    northDoor
  }

  /**
    * Check if the room has a east door
    *
    * @return   True if the door exists, false otherwise
    */
  def hasEastDoor: Boolean = {
    eastDoor
  }

  /**
    * Check if the room has a south door
    *
    * @return   True if the door exists, false otherwise
    */
  def hasSouthDoor: Boolean = {
    southDoor
  }

  /**
    * Check if the room has a west door
    *
    * @return   True if the door exists, false otherwise
    */
  def hasWestDoor: Boolean = {
    westDoor
  }

  /**
    * Set the door during game generation
    *
    * @return   True if the door was created successfully, false otherwise
    */
  private[kouhii] def openDoor(direction: Char): Boolean = {
    var success = true

    direction match {
      case 'N'|'n' =>
        northDoor = true
      case 'E'|'e' =>
        eastDoor = true
      case 'S'|'s' =>
        southDoor = true
      case 'W'|'w' =>
        westDoor = true
      case _ =>
        success = false
    }

    success
  }

  /**
    * Set the item in the room during game generation
    *
    * @return   True if the item was set successfully, false otherwise
    */
  private[kouhii] def setItem(i: Item): Boolean = {
    var success = true

    try {
      item = i
    }
    catch {
      case e: Exception => success = false
    }

    success
  }

  /**
    * Gets a description of a close inspection of the room
    *
    * @return   A string describing the room when closely examined
    */
  def inspect: String

  // Override the default toString method
  override def toString: String = {
    s"$description $roomType"
  }
}

class Kitchen extends Room {
  protected var roomType: String = "kitchen"

  // Randomly select a kitchen descriptor from the list
  protected var description: String = {
    val kitchenList = List[String](
      "fancy", "simple", "old-fashioned", "hi-tech", "confusing", "dilapidated",
      "run-down", "clean", "busy", "lovely", "welcoming", "hot"
    )
    val index = GameItems.nextInt(kitchenList.length)

    kitchenList(index)
  }

  def inspect: String = {
    "If you can't stand the heat..."
  }
}

class Bathroom extends Room {
  protected var roomType: String = "bathroom"

  // Randomly select a bathroom descriptor from the list
  protected var description: String = {
    val bathroomList = List[String](
      "clean", "disgusting", "messy", "foul-smelling", "spotless", "tidy"
    )
    val index = GameItems.nextInt(bathroomList.length)

    bathroomList(index)
  }

  def inspect: String = {
    "There's a jacuzzi in here!"
  }
}

class PowderRoom extends Room {
  protected var roomType: String = "powder room"

  // Randomly select a powder room descriptor from the list
  protected var description: String = {
    val powderList = List[String](
      "clean", "dirty", "grimy", "spotless", "fresh", "aromatic"
    )
    val index = GameItems.nextInt(powderList.length)

    powderList(index)
  }

  def inspect: String = {
    "Well, it's no Pittsburgh toilet..."
  }
}

class LivingRoom extends Room {
  protected var roomType: String = "living room"

  // Randomly select a living room descriptor from the list
  protected var description: String = {
    val livingList = List[String](
      "spacious", "cramped", "cozy", "comfy", "dirty", "warm", "enormous"
    )
    val index = GameItems.nextInt(livingList.length)

    livingList(index)
  }

  def inspect: String = {
    "That sofa looks quaint."
  }
}

class Library extends Room {
  protected var roomType: String = "library"

  // Randomly select a library descriptor from the list
  protected var description: String = {
    val libraryList = List[String](
      "empty", "filled", "quiet", "sunlit", "peaceful", "abandoned", "shady",
      "wondrous", "knowledgeable"
    )
    val index = GameItems.nextInt(libraryList.length)

    libraryList(index)
  }
  
  // Randomly select a book from the shelf
  private val bookList = List[String](
    "How to Train Your Dragon", "A Friendly Introduction to Software Testing",
    "War and Peace", "The Hungry Caterpillar", "Green Eggs and Ham",
    "Life of Pi"
  )
  private val book = bookList(GameItems.nextInt(bookList.length))

  def inspect: String = {
    s"""There's a book here called \"$book\"."""
  }
}

class Bedroom extends Room {
  protected var roomType: String = "bedroom"

  // Randomly select a bedroom descriptor from the list
  protected var description: String = {
    val bedroomList = List[String](
      "comfy", "dark", "moldy", "unkempt", "messy", "tidy", "clean", "neat",
      "cozy", "cold", "frigid", "sweltering", "warm", "inviting"
    )
    val index = GameItems.nextInt(bedroomList.length)

    bedroomList(index)
  }

  def inspect: String = {
    "I could just nap right here..."
  }
}

class MasterBedroom extends Room {
  protected var roomType: String = "master bedroom"

  // Randomly select a master bedroom descriptor from the list
  protected var description: String = {
    val bedroomList = List[String](
      "comfy", "large", "spacious", "invigorating", "sunlit", "inviting",
      "neat", "tidy", "clean", "beautiful"
    )
    val index = GameItems.nextInt(bedroomList.length)

    bedroomList(index)
  }

  def inspect: String = {
    "That's an impressive bed."
  }
}

class GuestBedroom extends Room {
  protected var roomType: String = "guest bedroom"

  // Randomly select a guest bedroom descriptor from the list
  protected var description: String = {
    val bedroomList = List[String](
      "cramped", "dark", "cozy", "uninviting", "neat", "small", "official-looking",
      "busy"
    )
    val index = GameItems.nextInt(bedroomList.length)

    bedroomList(index)
  }

  def inspect: String = {
    "Looks more like an office than a bedroom..."
  }
}

class GameRoom extends Room {
  protected var roomType: String = "game room"
  
  // Randomly select a game room descriptor from the list
  protected var description: String = {
    val gameList = List[String](
      "retro", "modern", "inspiring", "glamorous", "wild", "crazy", "fun",
      "well-maintained", "futuristic"
    )
    val index = GameItems.nextInt(gameList.length)

    gameList(index)
  }
  
  // Randomly select a game from the list
  private val gameList = List[String](
    "Super Mario Bros.", "Halo: Combat Evolved", "Final Fantasy VI",
    "Final Fantasy IX", "The Witcher 3: Wild Hunt", "The Legend of Zelda: Ocarina of Time",
    "Sonic Adventure 2", "Nights into Dreams", "Banjo-Kazooie", "Armored Core",
    "Power Stone 2", "Pokémon Yellow", "Roller Coaster Tycoon", "Forza Motorsport 4"
  )
  private val game = gameList(GameItems.nextInt(gameList.length))

  def inspect: String = {
    s"""Look! There's a copy of \"$game\" here!"""
  }
}

class MusicStudy extends Room {
  protected var roomType: String = "music study"
  
  // Randomly select a music study descriptor from the list
  protected var description: String = {
    val musicList = List[String](
      "fascinating", "inspiring", "joyous", "whimsical", "grand", "serious"
    )
    val index = GameItems.nextInt(musicList.length)

    musicList(index)
  }

  def inspect: String = {
    "Just itching to tickle those ivories!"
  }
}

class Foyer extends Room {
  protected var roomType: String = "foyer"
  
  // Randomly select a foyer descriptor from the list
  protected var description: String = {
    val foyerList = List[String](
      "grand", "vast", "spacious", "wide-open", "puny", "dark", "damp",
      "damaged", "pristine"
    )
    val index = GameItems.nextInt(foyerList.length)

    foyerList(index)
  }

  def inspect: String = {
    "Hmm, where should I go next?"
  }
}

class HomeTheater extends Room {
  protected var roomType: String = "home theater"
  
  // Randomly select a home theater descriptor from the list
  protected var description: String = {
    val theaterList = List[String](
      "classic", "well-done", "beautiful", "impressive", "tiny", "loud",
      "dramatic", "poorly-planned"
    )
    val index = GameItems.nextInt(theaterList.length)

    theaterList(index)
  }

  def inspect: String = {
    "If only this was Popcorn Maker Quest..."
  }
}

class Bar extends Room {
  protected var roomType: String = "bar"
  
  // Randomly select a bar descriptor from the list
  protected var description: String = {
    val barList = List[String](
      "smoky", "unappealing", "haggard", "orderly", "busy", "packed", "drenched",
      "slippery", "shady"
    )
    val index = GameItems.nextInt(barList.length)

    barList(index)
  }
  
  // Randomly order a drink from the menu
  private val drinkList = List[String](
    "Shirley Temple", "rum & Coke", "rum runner", "gin & tonic", "margarita",
    "pina colada", "martini", "glass of whiskey", "glass of scotch",
    "Irish coffee", "bloody Mary", "pop"
  )

  def inspect: String = {
    s"I should fix me a ${drinkList(GameItems.nextInt(drinkList.length))}."
  }
}

class Pool extends Room {
  protected var roomType: String = "indoor pool"
  
  // Randomly select a pool descriptor from the list
  protected var description: String = {
    val poolList = List[String](
      "wet", "cold", "freezing", "humid", "hot", "steamy", "cavernous", "loud",
      "vast"
    )
    val index = GameItems.nextInt(poolList.length)

    poolList(index)
  }

  def inspect: String = {
    "My feet are soaked!"
  }
}

class Garage extends Room {
  protected var roomType: String = "garage"
  
  // Randomly select a garage descriptor from the list
  protected var description: String = {
    val garageList = List[String](
      "noisy", "tidy", "loud", "greasy", "shiny", "quiet", "packed", "tight",
      "spacious", "awesome"
    )
    val index = GameItems.nextInt(garageList.length)

    garageList(index)
  }
  
  // Randomly select a car from the list
  private val garageList = List[String](
    "Aston Martin DB5", "Aston Martin DB9", "Audi R8 V10 Spyder", "BMW M3",
    "BMW M6", "Bugatti Chiron", "Datsun 240Z", "Datsun 280ZX",
    "De Tomaso Pantera", "Dodge Viper ACR", "Ferrari 330 P4",
    "Ferrari Enzo Ferrari", "Ferrari F40", "Ferrari Testarossa",
    "Honda NSX Type-R", "Honda S2000", "Jaguar C-X75", "Jaguar XJ220",
    "Jaguar XKR-S GT", "Lamborghini Diablo", "Lamborghini Gallardo Superleggera",
    "Lamborghini Miura", "Lamborghini Jalpa", "Lancia Stratos", "Mazda RX-7",
    "Mercedes-AMG GT", "Mercedes-Benz SLS AMG", "McLaren F1",
    "Mitsubishi Lancer Evolution", "Nissan Skyline GT-R", "Pagani Huayra BC",
    "Pagani Zonda Cinque", "Porsche 911 Turbo", "Porsche 918", "Porsche 928",
    "Porsche Carrera GT", "Saleen S7", "Shelby GT350-R", "Subaru Impreza WRX STI",
    "Subaru SVX", "Toyota 2000GT", "Toyota Supra"
  )
  private val car = garageList(GameItems.nextInt(garageList.length))

  def inspect: String = {
    s"There's an absolutely mint $car just sitting here!"
  }
}
