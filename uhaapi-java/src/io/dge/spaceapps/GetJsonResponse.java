package io.dge.spaceapps;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

class GetJsonResponse extends JsonResponse {
    private static final String MIME_JSON = "application/json";

    public GetJsonResponse(int responseCode, String body) {
        super(responseCode, body);
    }

    public static GetJsonResponse makeRequest (String resource)
            throws IOException {
        HttpURLConnection connection =
                (HttpURLConnection) new URL(resource).openConnection();

        try {
            connection.setRequestProperty("Accept", MIME_JSON);

            InputStream in = new BufferedInputStream(connection.getInputStream());
            String responseBody = Utils.inputStreamToString(in);
            in.close();

            return new GetJsonResponse(connection.getResponseCode(), responseBody);
        } finally {
            connection.disconnect();
        }
    }
}
