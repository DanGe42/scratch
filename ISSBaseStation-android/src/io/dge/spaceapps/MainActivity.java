package io.dge.spaceapps;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Intent;
import android.os.Bundle;
import android.view.*;
import android.widget.Button;

public class MainActivity extends Activity {
    private boolean isShowingMap;
    private Button toggleButton;

    private static final String TRACKER_ID = "tracker",
                                TEST_ID    = "test";

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().requestFeature(Window.FEATURE_PROGRESS);

        setContentView(R.layout.main);

        toggleButton = (Button) findViewById(R.id.change_view);

        isShowingMap = true;
        FragmentManager manager = getFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        ft.replace(R.id.fragment_container, new ISSTrackerFragment(), TRACKER_ID);
        ft.commit();
    }

    public void toggleView (View _) {
        FragmentManager manager = getFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        ft.addToBackStack(null);

        if (isShowingMap) {
            Fragment base = manager.findFragmentByTag(TEST_ID);
            if (base != null) {
                ft.remove(base);
            } else {
                base = new TestBaseFragment();
            }

            ft.replace(R.id.fragment_container, base, TEST_ID);
            toggleButton.setText("Map");
        } else {
            Fragment tracker = manager.findFragmentByTag(TRACKER_ID);
            if (tracker != null) {
                ft.remove(tracker);
            } else {
                tracker = new ISSTrackerFragment();
            }

            ft.replace(R.id.fragment_container, tracker);
            toggleButton.setText(getResources().getText(R.string.change_view_text));
        }
        ft.commit();

        isShowingMap = !isShowingMap;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_toolbar, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.prefs_item:
                startActivity(new Intent(this, PrefsActivity.class));
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}
