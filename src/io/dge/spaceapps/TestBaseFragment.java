package io.dge.spaceapps;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import java.io.IOException;

public class TestBaseFragment extends Fragment {
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle
            savedInstanceState) {
        final View view = inflater.inflate(R.layout.test_base, container, false);

        Button submit = (Button) view.findViewById(R.id.submit_button);
        submit.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                EditText azEdit = (EditText) view.findViewById(R.id.az_edit);
                EditText elevEdit = (EditText) view.findViewById(R.id.elev_edit);

                int azimuth = Integer.parseInt(azEdit.getText().toString());
                int elevation = Integer.parseInt(elevEdit.getText().toString());

                String hostname = ((BaseStationApp) getActivity().getApplication())
                        .getPrefs().getString("base_hostname", "");

                try {
                    BaseStation.sendData(hostname, azimuth, elevation);
                } catch (IOException e) {
                    Toast.makeText(getActivity(), e.getMessage(), Toast.LENGTH_SHORT).show();
                }
            }
        });
        return view;
    }
}
