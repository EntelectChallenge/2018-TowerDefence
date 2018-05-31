package za.co.entelect.challenge

import com.google.gson.Gson

import java.io.*

object Main {
    private const val COMMAND_FILE_NAME = "command.txt"
    private const val STATE_FILE_NAME = "state.json"

    @JvmStatic
    fun main(args: Array<String>) {
        var state = ""
        try {
            state = File(STATE_FILE_NAME).bufferedReader().use {
                it.readLine()
            }
        } catch (e: IOException) {
            e.printStackTrace()
        }

        val gameState = Gson().fromJson(state, GameState::class.java)

        val bot = Bot(gameState)
        val command = bot.run()

        writeBotResponseToFile(command)
    }

    private fun writeBotResponseToFile(command: String) {
        try {
            File(COMMAND_FILE_NAME).bufferedWriter().use {
                it.write(command)
            }
        } catch (e: IOException) {
            e.printStackTrace()
        }

    }
}
