package io.dge.spaceapps;

import android.app.Application;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class BaseStationApp extends Application{
    private SharedPreferences prefs;

    @Override
    public void onCreate() {
        super.onCreate();
    }

    public SharedPreferences getPrefs() {
        if (prefs == null) {
            prefs = PreferenceManager.getDefaultSharedPreferences(this);
            return prefs;
        } else {
           return prefs;
        }
    }
}
