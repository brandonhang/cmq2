package kouhii

/**
  * Base trait used to define items in the game
  */
trait Item {
  protected var itemType: String
  protected var description: String

  /**
    * Gets the item's type
    *
    * @return   A string of the item's type
    */
  def isType: String = {
    itemType
  }

  /**
    * Gets a description of the item
    *
    * @return   A string describing the item
    */
  def describe: String = {
    description
  }

  /**
    * Gets a description of a close inspection of the item
    *
    * @return   A string describing the item when closely examined
    */
  def inspect: String

  // Override the default toString method
  override def toString: String = {
    s"$description $itemType"
  }
}

class Coffee extends Item {
  protected var itemType: String = "coffee"

  // Randomly select a type of coffee from the list
  protected var description: String = {
    val kouhiiList = List[String](
      "Jamaican", "Colombian", "Hawaiian", "Brazilian", "Vietnamese"
    )
    val index = GameItems.nextInt(kouhiiList.length)

    kouhiiList(index)
  }

  def inspect: String = {
    "Java is a programmer's best friend."
  }
}

class Cream extends Item {
  protected var itemType: String = "cream"

  // Randomly select a type of creamer from the list
  protected var description: String = {
    val creamList = List[String](
      "hazelnut", "French vanilla", "pumpkin spiced", "half and half", "spoiled"
    )
    val index = GameItems.nextInt(creamList.length)

    creamList(index)
  }

  def inspect: String = {
    "Who just leaves cream laying around?"
  }
}

class Sugar extends Item {
  protected var itemType: String = "sugar"

  // Randomly select a type of sugar from the list
  protected var description: String = {
    val sugarList = List[String](
      "cane", "substitute", "pure", "powdered", "rock"
    )

    val index = GameItems.nextInt(sugarList.length)

    sugarList(index)
  }

  def inspect: String = {
    "You can just feel the sugary rush!"
  }
}

class CoffeeMachine extends Item {
  protected var itemType: String = "coffee machine"

  // Randomly select a coffee machine descriptor from the list
  protected var description: String = {
    val machineList = List[String](
      "fancy", "worn", "sturdy", "steampunk", "retro",
      "slightly broken but still working", "smoking", "simple"
    )

    val index = GameItems.nextInt(machineList.length)

    machineList(index)
  }

  def inspect: String = {
    "Where's the ON button to this thing?"
  }
}

class CoffeeFilter extends Item {
  protected var itemType: String = "coffee filter"

  // Randomly select a type of coffee filter from the list
  protected var description: String = {
    val filterList = List[String](
      "simple paper", "regular paper", "paper", "reusable metal"
    )

    val index = GameItems.nextInt(filterList.length)

    filterList(index)
  }

  def inspect: String = {
    "No grounds allowed!"
  }
}

class CoffeeGrinder extends Item {
  protected var itemType: String = "coffee grinder"

  // Randomly select a coffee machine descriptor from the list
  protected var description: String = {
    val grinderList = List[String](
      "portable", "fancy", "slightly broken but still working", "state-of-the-art",
      "expensive", "dull", "colorful", "noisy"
    )

    val index = GameItems.nextInt(grinderList.length)

    grinderList(index)
  }

  def inspect: String = {
    "Better try not to make a mess..."
  }
}

class Mug extends Item {
  protected var itemType: String = "mug"

  // Randomly select a type of mug from the list
  protected var description: String = {
    val mugList = List[String](
      "unadorned", "small", "giant", "glass", "artistic", "cheap", "massive",
      "simple", "colorful", "whimsical", "controversial", "stolen", "cracked",
      "used", "dirty"
    )
    val index = GameItems.nextInt(mugList.length)

    mugList(index)
  }

  def inspect: String = {
    "It holds liquids so you don't have to!"
  }
}

class Spoon extends Item {
  protected var itemType: String = "spoon"

  // Randomly select a type of spoon from the list
  protected var description: String = {
    val spoonList = List[String](
      "dirty", "rusty", "clean", "shiny", "silver", "gold", "copper", "plastic",
      "filthy", "bardy", "tiny", "oversized", "unremarkable", "dull"
    )
    val index = GameItems.nextInt(spoonList.length)

    spoonList(index)
  }

  def inspect: String = {
    "There is no spoon."
  }
}
  