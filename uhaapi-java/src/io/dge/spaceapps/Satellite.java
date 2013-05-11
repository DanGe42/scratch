package io.dge.spaceapps;

import java.io.IOException;

public class Satellite {
    public static final int ISS = 25544;

    private static final String API_ROOT = "http://api.uhaapi.com/satellites/";
    private long launched;
    private int id;
    private String idc, name;

    private Satellite(){}

    public static Satellite getSatellite (int id)
            throws IOException, InternalServerException {
        JsonResponse response =
                GetJsonResponse.makeRequest(API_ROOT + String.valueOf(id));

        switch (response.getResponseCode()) {
            case 200:
                Satellite sat = response.fromJson(Satellite.class);
                return sat;
            case 404:
                return null;
            default:
                throw new InternalServerException();
        }
    }

    public long getLaunchDate() {
        return launched;
    }

    public int getId() {
        return id;
    }

    public String getLongId() {
        return idc;
    }

    public String getName() {
        return name;
    }
}
