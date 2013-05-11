package io.dge.spaceapps;

import java.io.IOException;

public class SatellitePasses {
    private static final String API_ROOT =
            "http://api.uhaapi.com/satellites/%d/passes?lat=%f&lng=%f";

    /* Data for GSON to serialize into */
    private int id;
    private Location location;

    private class Location {
        private double lat;
        private double lng;

        private Location(){}
    }

    private double altitude;
    private long from, to;

    private Pass[] results;

    private class Pass {
        private float magnitude;

        private class PassType {
            private long time;
            private double alt;
            private double az;

            private PassType(){}

            public long getTime() {
                return time;
            }

            public double getAltitude() {
                return alt;
            }

            public double getAzimuth() {
                return az;
            }
        }

        private PassType start, max, end;
        private Pass(){}

        public float getMagnitude() {
            return magnitude;
        }

        private PassType getStart() {
            return start;
        }

        private PassType getMax() {
            return max;
        }

        private PassType getEnd() {
            return end;
        }
    }


    private SatellitePasses (){}

    public int getID() {
        return id;
    }

    public double getMyLatitude() {
        return location.lat;
    }

    public double getMyLongitude() {
        return location.lng;
    }

    public double getMyElevation() {
        return altitude;
    }

    public Pass[] getPasses() {
        return results;
    }


    public static SatellitePasses getSatellitePasses (int id, double lat, double lng)
            throws IOException, InternalServerException {
        String requestUrl = String.format(API_ROOT, id, lat, lng);
        JsonResponse response =
                GetJsonResponse.makeRequest(requestUrl);

        switch (response.getResponseCode()) {
            case 200:
                SatellitePasses sat = response.fromJson(SatellitePasses.class);
                return sat;
            case 404:
                return null;
            default:
                throw new InternalServerException();
        }
    }
}
