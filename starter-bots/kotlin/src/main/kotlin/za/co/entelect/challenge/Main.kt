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
            val br = BufferedReader(FileReader(STATE_FILE_NAME))
            state = br.readLine()
        } catch (e: IOException) {
            e.printStackTrace()
        }

        val gson = Gson()
        val gameState = gson.fromJson(state, GameState::class.java)

        val bot = Bot(gameState)
        val command = bot.run()

        writeBotResponseToFile(command)
    }

    private fun writeBotResponseToFile(command: String) {
        try {
            val bufferedWriter = BufferedWriter(FileWriter(File(COMMAND_FILE_NAME)))
            bufferedWriter.write(command)
            bufferedWriter.flush()
            bufferedWriter.close()
        } catch (e: IOException) {
            e.printStackTrace()
        }

    }
}
