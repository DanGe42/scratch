package io.dge.spaceapps;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class BaseStation {
    public static void sendData (String hostname, double azimuth, double elevation)
            throws IOException {
        String path = String.format("%s/?%d,%d,1", hostname, (int) azimuth,
                (int) elevation);
        HttpURLConnection connection =
                (HttpURLConnection) new URL(path).openConnection();

        try {
            InputStream in = new BufferedInputStream(connection.getInputStream());
            String responseBody = Utils.inputStreamToString(in);
            in.close();
        } finally {
            connection.disconnect();
        }
    }
}
