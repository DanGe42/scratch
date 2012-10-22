package org.danielge.nfccard;

import android.content.Intent;
import android.content.IntentFilter;
import android.nfc.NdefMessage;
import android.nfc.NdefRecord;
import android.nfc.NfcAdapter;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;
import org.danielge.nfcskeleton.NdefReaderActivity;

import java.nio.charset.Charset;

public class CardReaderActivity extends NdefReaderActivity {
    public static final String TAG = CardReaderActivity.class.getSimpleName();

    private static final Charset charset = Charset.forName("US-ASCII");

    private final IntentFilter[] FILTERS = new IntentFilter[] { buildIntentFilter() };

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.reader);
    }

    @Override
    protected void onResume() {
        super.onResume();
        enableReadTagMode(FILTERS);
    }

    @Override
    protected void onNdefMessage(NdefMessage message) {
        clearAllTextViews();

        NdefRecord[] records = message.getRecords();
        NdefRecord fieldRecord = records[0];
        NdefRecord descRecord = records[1];
        // NdefRecord metaRecord = records[2];

        if (!parseFieldsFromRecord(fieldRecord)) {
            Toast.makeText(this, "Tag not formatted correctly!", Toast.LENGTH_LONG).show();
            return;
        }

        if (!parseDescription(descRecord)) {
            Toast.makeText(this, "Could not read description", Toast.LENGTH_SHORT).show();
            return;
        }

        Toast.makeText(this, "Finished reading tag", Toast.LENGTH_SHORT).show();
    }

    private boolean parseFieldsFromRecord (NdefRecord record) {

        if (record.getTnf() != NdefRecord.TNF_MIME_MEDIA
                || !CardUtils.MIME.equals(new String(record.getType(), charset))) {
            return false;
        }

        String payload = new String(record.getPayload(), charset);

        for (String entry : payload.split(CardUtils.SEPARATOR)) {
            // 2 is the length of the minimum entry (x=)
            if (entry == null || entry.length() <= 2)
                continue;

            String[] keyValue = entry.split("=");
            if (keyValue.length != 2)
                continue;

            TextView textView = (TextView) findViewById(CardUtils.decodeFromReader(keyValue[0]));
            textView.setText(keyValue[1]);
        }

        return true;
    }

    private boolean parseDescription (NdefRecord record) {
        if (record.getTnf() != NdefRecord.TNF_MIME_MEDIA
                || !"text/plain".equals(new String(record.getType(), charset))) {
            return false;
        }

        String payload = new String(record.getPayload(), charset);
        TextView textView = (TextView) findViewById(R.id.desc_value);
        textView.setText(payload);

        return true;
    }

    private void clearAllTextViews() {
        for (int textView : CardUtils.getReaderViews()) {
            TextView tv = (TextView) findViewById(textView);
            tv.setText("");
        }
    }

    public void editTagAction (View v) {
        // Intent.putExtras
        Intent intent = new Intent(this, CardWriterActivity.class);
        intent.setAction(Intent.ACTION_VIEW);
        intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        intent.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);

        for (int tvId : CardUtils.getReaderViews()) {
            TextView textView = (TextView) findViewById(tvId);

            intent.putExtra(CardUtils.encodeFromReader(textView), textView.getText());
        }

        startActivity(intent);
    }

    private IntentFilter buildIntentFilter() {
        IntentFilter filter = new IntentFilter();
        filter.addAction(NfcAdapter.ACTION_NDEF_DISCOVERED);
        filter.addCategory(Intent.CATEGORY_DEFAULT);
        try {
            filter.addDataType(CardUtils.MIME);
        } catch (IntentFilter.MalformedMimeTypeException e) {
            Log.wtf(TAG, "Invalid MIME type", e);
            Toast.makeText(this, "Something went wrong. Please contact your local developer",
                    Toast.LENGTH_LONG).show();
        }

        return filter;
    }
}