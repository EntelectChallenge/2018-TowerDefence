package za.co.entelect.challenge.player;

import okhttp3.OkHttpClient;
import okhttp3.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;
import za.co.entelect.challenge.botrunners.BotRunner;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.enums.BotLanguage;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.IOException;

public class TournamentPlayer extends Player {

    private BotRunner botRunner;
    private BotLanguage language;
    private BotServices botServices;

    public TournamentPlayer(String name, BotRunner runner, BotLanguage botLanguage) {
        super(name);

        botRunner = runner;
        language = botLanguage;

        String dockerUrl = String.format("https://localhost:%d", botRunner.getDockerPort());

        OkHttpClient.Builder httpClient = new OkHttpClient.Builder();
        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl(dockerUrl)
                .addConverterFactory(GsonConverterFactory.create())
                .client(httpClient.build())
                .build();

        botServices = retrofit.create(BotServices.class);
    }

    @Override
    public void startGame(GameMap gameMap) {
        newRoundStarted(gameMap);
    }

    @Override
    public void newRoundStarted(GameMap gameMap) {

        RawCommand rawCommand = null;
        try {
            Response response = botServices.runBot(FileUtils.getAbsolutePath(botRunner.getBotDirectory()), botRunner.getBotFileName(), language.name());
            if (response.isSuccessful()) {
                rawCommand = new RawCommand(response.body().string());
            } else {
                rawCommand = new RawCommand("No command");
            }
        }

        catch (IOException e) {
            e.printStackTrace();
            rawCommand = new RawCommand("No command");
        }

        publishCommand(rawCommand);
    }

    @Override
    public void gameEnded(GameMap gameMap) {

    }
}
