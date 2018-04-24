package za.co.entelect.challenge;

import com.google.gson.Gson;
import za.co.entelect.challenge.entities.GameState;

import java.io.*;

public class Main {
    private static final String COMMAND_FILE_NAME = "command.txt";
    private static final String STATE_FILE_NAME = "state.json";

    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();

        String state = "";
        try {
            BufferedReader br = new BufferedReader(new FileReader(STATE_FILE_NAME));
            StringBuilder sb = new StringBuilder();
            state = br.readLine();
        }catch (IOException e){
            e.printStackTrace();
        }

        Gson gson = new Gson();
        GameState gameState = gson.fromJson(state, GameState.class);

        Bot bot = new Bot(gameState);
        String command = bot.Run();
        System.out.println(command);

        writeBotResponseToFile(command);

        long stopTime = System.currentTimeMillis();
        long elapsedTime = stopTime - startTime;
        System.out.println("Elapsed Time in milis: " + elapsedTime);
    }

    private static void writeBotResponseToFile(String command) {
        try {
            BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(new File(COMMAND_FILE_NAME)));
            bufferedWriter.write(command);
            bufferedWriter.flush();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
