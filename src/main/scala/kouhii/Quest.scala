package kouhii

import scala.io.StdIn

/**
  * Game logic
  */
class Quest {
  val building = new Building
  var room: Room = null
  var continue = true
  var inventory: Map[String, Item] = Map()

  while (continue) {
    room = building.getCurrentRoom

    println("\033[2J")
    println(building.viewMap)
    println(s"You are in a $room.")

    var moving = false

    // Get user input for game interaction
    while (!moving) {
      println("What will you do?")
      print("\033[4mL\033[0mook, \033[4mI\033[0mnventory, \033[4mM\033[map, ")

      if (inventory.size  == building.getNumItems) {
        print("\033[4mD\033[0mrink, ")
      }

      print(s"\033[4mN\033[0morth, \033[4mE\033[0mast, ")
      print("\033[4mS\033[0mouth, \033[4mW\033[0mest: \n\n\033[A")

      moving = getInput(StdIn.readLine())
    }
  }

  /**
    * Parse the user input to perform a game action
    *
    * @param input      The input the user entered
    * @return           True if the user moved to a new room, false otherwise
    */
  private def getInput(input: String): Boolean = {
    var moved = false

    print("\033[A\033[2K" * 4)
    input.toUpperCase match {
      // Match the cardinal directions; return true if a door is available
      case "N"|"E"|"S"|"W"|"NORTH"|"EAST"|"SOUTH"|"WEST" =>
        val direction = input.toUpperCase.charAt(0)

        moved = direction match {
          case 'N' =>
            if (room.hasNorthDoor) {
              building.moveNorth
            }
            else {
              false
            }
          case 'E' =>
            if (room.hasEastDoor) {
              building.moveEast
            }
            else {
              false
            }
          case 'S' =>
            if (room.hasSouthDoor) {
              building.moveSouth
            }
            else {
              false
            }
          case _ =>
            if (room.hasWestDoor) {
              building.moveWest
            }
            else {
              false
            }
        }

        if (!moved) {
          println("Try as you might, there is no door there.")
        }
      // Look around the room for an item
      case "L"|"LOOK" =>
        print(s"${room.inspect}  ")

        if (room.hasItem) {
          val item: Item = room.takeItem
          inventory += (item.isType -> item)
          println(s"You found a${
            if ("""^[AEIOUaeiou]""".r.findFirstIn(item.describe).isDefined) "n" else ""
          } $item.")
        }
        else {
          println("You find nothing of interest.")
        }
      // Look at the player's current inventory
      case "I"|"INVENTORY" =>
        if (inventory.isEmpty) {
          println("You look in your jumbo-sized bag but it's completely barren.")
        }
        else {
          println("\033[2JYou peek into your monstrously large bag...")

          for ((key, item) <- inventory) {
            println(s"1 x $item -- ${item.inspect}")
          }
        }
      // Reprint the map of the building
      case "M"|"MAP" =>
        println("\033[2J")
        println(building.viewMap)
        println(s"You are in a${
          if ("""^[AEIOUaeiou]""".r.findFirstIn(room.describe).isDefined) "n" else ""
        }$room.")
      // Drink the coffee if all the items are available
      case "D"|"DRINK" =>
        if (inventory.size  == building.getNumItems) {
          if (room.isType.equals("kitchen") || room.isType.equals("bar")) {
            print("Let's get to work!  You clear out a section of the counter ")
            print(s"and plug in the ${inventory("coffee grinder")}.  You ")
            print(s"carefully pour in some ${inventory("coffee")} beans and ")
            print("grind the it into a fine powder.  You empty the coffee ")
            print(s"grounds into the ${inventory("coffee filter")} and place ")
            print(s"the filter into the ${inventory("coffee machine")}.  You ")
            print(s"pull out your ${inventory("mug")} and fill it with some ")
            print("fresh water from the nearby sink.  You pour the water into ")
            print(s"the ${inventory("coffee machine")}, plug it in, and turn it ")
            print("on.  *Bzzz* *Drip* *Drip*  The coffee is ready to pour!  You ")
            print(s"fill your ${inventory("mug")} with some delicious ")
            print(s"${inventory("coffee")} and add some ${inventory("cream")} ")
            print(s"and ${inventory("sugar")} to it.  After stirring it with ")
            print(s"your ${inventory("spoon")}, you take a sip of your freshly ")
            println("brewed coffee.  Mmm, delicious!")
            println("\033[0;32mYou win!\033[0m")
            println("\n\033[0;36mGAME OVER\033[0m\n")
            moved = true
            continue = false
          }
          else if (room.isType.equals("bathroom") || room.isType.equals("powder room")) {
            print("You figure the bathroom is as good a place as any to make ")
            print("coffee.  Slightly repulsed but determined, you plug in the ")
            print(s"${inventory("coffee grinder")} and pour some ")
            print(s"${inventory("coffee")} beans into it.  Grinding it into a ")
            print("fine powder, you empty the grounds into your ")
            print(s"${inventory("coffee filter")} and place it into the ")
            print(s"${inventory("coffee machine")}.  You then fill the ")
            print(s"${inventory("mug")} you found with some water from the sink.  ")
            print("Oh-no!  The sink won't run!  ")
            if (room.isType.equals("bathroom")) {
              print("You frantically try turning on the shower and the ")
              print("jacuzzi, but neither one runs!  ")
            }
            print("That makes the only remaining source of water the toilet...  ")
            print("You shudder even thinking about it but you are desperate for ")
            print(s"coffee.  Trembling slightly, you dip your ${inventory("mug")} ")
            print("into the toilet, draw out some scummy water, and pour it into ")
            print(s"the ${inventory("coffee machine")}.  It has to filter out, ")
            print("right?  Right!?  You power the machine on.  *Bzzz* *Drip* ")
            print(s"*Drip*  You pour out some unhygienic ${inventory("coffee")} ")
            print(s"and stir in some ${inventory("cream")} and ")
            print(s"${inventory("sugar")} using your ${inventory("spoon")}.  ")
            print("You prepare yourself for this moment, but the moment is quite ")
            print("difficult.  You force yourself to drink this vile concoction.  ")
            print("Blech!  It tastes like used toilet paper!  You slam your ")
            print(s"${inventory("mug")} down in disgust but it's too late.  You ")
            println("already feel seriously ill...")
            println("\033[0;31mYou lose.\033[0m")
            println("\n\033[0;36mGAME OVER\033[0m\n")
            moved = true
            continue = false
          }
          else if (room.isType.equals("indoor pool")) {
            print("You suppose you could make some coffee in the pool.  You pull ")
            print(s"out your ${inventory("coffee grinder")} and fill it with some ")
            print(s"${inventory("coffee")} beans.  After grinding into a somewhat ")
            print(s"coarse powder, you place the ${inventory("coffee")} grounds ")
            print(s"into the ${inventory("coffee filter")} and clumsily place ")
            print(s"it inside the ${inventory("coffee machine")}.  You're making ")
            print("a huge mess!  You walk over towards the edge of the pool and ")
            print(s"fill your ${inventory("mug")} with some funky-smelling pool ")
            print("water.  The chlorine should mean it's clean, right?  Eh, maybe.  ")
            print("You walk back to your makeshift station, pour the chemically-")
            print(s"fortified water into the ${inventory("coffee machine")} and ")
            print("start it up.  *Bzzz* *Drip* *Drip*  This coffee doesn't smell ")
            print("right, but you don't care.  You NEED this coffee.  Steeling ")
            print("yourself for what's to come, you stir in some ")
            print(s"${inventory("cream")} and ${inventory("sugar")} with the ")
            print(s"${inventory("spoon")} you acquired and take a hearty gulp from ")
            print(s"your ${inventory("mug")}.  It doesn't taste half bad...  ")
            print(s"You suddenly feel weak and drop your ${inventory("mug")}.  It ")
            println("shatters on the tile below.  You feel very sick...")
            println("\033[0;31mYou lose.\033[0m")
            println("\n\033[0;36mGAME OVER\033[0m\n")
            moved = true
            continue = false
          }
          else {
            println("You can't make coffee here, you need water!")
          }
        }
        // Can only drink if all the items are available
        else {
          println("You become disoriented but the moment quickly passes.")
        }
      // Default case where no commands match
      case _ =>
        println("You become disoriented but the moment quickly passes.")
    }

    moved
  }
}
