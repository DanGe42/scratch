package org.danielge.nfccard;

import android.view.View;

import java.util.HashMap;
import java.util.Map;

public class CardUtils {
    public static final String MIME = "application/org.danielge.nfccard";

    public static final String SEPARATOR = "\r\n";

    public static final String META = "BusinessCard\n(C) D.Ge\nv0.0.1";

    public static final int W_NAME     = R.id.name_input,
                            W_EMAIL    = R.id.email_input,
                            W_PHONE    = R.id.phone_input,
                            W_WORK     = R.id.work_school_input,
                            W_POSITION = R.id.position_major_input,
                            W_TWITTER  = R.id.twitter_input,
                            W_WEBSITE  = R.id.website_input,
                            W_DESC     = R.id.desc_input;

    public static final int R_NAME = R.id.name_value,
                            R_EMAIL = R.id.email_value,
                            R_PHONE = R.id.phone_value,
                            R_WORK = R.id.work_school_value,
                            R_POSITION = R.id.position_major_value,
                            R_TWITTER = R.id.twitter_value,
                            R_WEBSITE = R.id.website_value,
                            R_DESC = R.id.desc_value;

    private static final Map<Integer, String> writeViewToEncoding;
    private static final Map<String, Integer> encodingToWriteView;
    static {
        writeViewToEncoding = new HashMap<Integer, String>();
        writeViewToEncoding.put(W_NAME, "name");
        writeViewToEncoding.put(W_EMAIL, "email");
        writeViewToEncoding.put(W_PHONE, "phone");
        writeViewToEncoding.put(W_WORK, "work");
        writeViewToEncoding.put(W_POSITION, "pos");
        writeViewToEncoding.put(W_TWITTER, "twit");
        writeViewToEncoding.put(W_WEBSITE, "site");
        writeViewToEncoding.put(W_DESC, "desc");

        encodingToWriteView = reverseMap(writeViewToEncoding);
    }

    private static final Map<Integer, String> readViewToEncoding;
    private static final Map<String, Integer> encodingToReadView;
    static {
        readViewToEncoding = new HashMap<Integer, String>();
        readViewToEncoding.put(R_NAME, "name");
        readViewToEncoding.put(R_EMAIL, "email");
        readViewToEncoding.put(R_PHONE, "phone");
        readViewToEncoding.put(R_WORK, "work");
        readViewToEncoding.put(R_POSITION, "pos");
        readViewToEncoding.put(R_TWITTER, "twit");
        readViewToEncoding.put(R_WEBSITE, "site");
        readViewToEncoding.put(R_DESC, "desc");

        encodingToReadView = reverseMap(readViewToEncoding);
    }

    public static String encodeFromWriter(View v) {
        return writeViewToEncoding.get(v.getId());
    }

    public static int decodeFromWriter(String shortName) {
        return encodingToWriteView.get(shortName);
    }

    public static String encodeFromReader (View v) {
        return readViewToEncoding.get(v.getId());
    }

    public static int decodeFromReader (String shortName) {
        return encodingToReadView.get(shortName);
    }

    public static Iterable<Integer> getReaderViews() {
        return readViewToEncoding.keySet();
    }

    public static Iterable<Integer> getWriterViews() {
        return writeViewToEncoding.keySet();
    }

    private static<K, V> Map<V, K> reverseMap(Map<K, V> map) {
        Map<V,K> reverseMap = new HashMap<V, K>();
        for (Map.Entry<K, V> entry : map.entrySet()) {
            reverseMap.put(entry.getValue(), entry.getKey());
        }

        return reverseMap;
    }
}
