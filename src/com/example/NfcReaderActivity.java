package com.example;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.nfc.NdefMessage;
import android.nfc.NdefRecord;
import android.nfc.NfcAdapter;
import android.nfc.Tag;
import android.nfc.tech.Ndef;
import android.os.Bundle;
import android.os.Parcelable;
import android.util.Log;
import android.widget.TextView;
import android.widget.Toast;

import java.nio.charset.Charset;

/**
 * Created with IntelliJ IDEA.
 * User: danielge
 * Date: 10/18/12
 * Time: 2:27 AM
 * To change this template use File | Settings | File Templates.
 */
public class NfcReaderActivity extends Activity {
    public static final String TAG = NfcReaderActivity.class.getSimpleName();

    private TextView idText, noteText;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.reader);

        idText = (TextView) findViewById(R.id.id_text);
        noteText = (TextView) findViewById(R.id.note_text);
    }

    @Override
    protected void onResume() {
        super.onResume();

        enableWaitForTag();

        Intent intent = getIntent();
        if (processNdefIntent(intent)) {
            Toast.makeText(this, "Successfully read tag", Toast.LENGTH_SHORT).show();
        } else {
            Toast.makeText(this, "Could not read tag", Toast.LENGTH_SHORT).show();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();

        disableWaitForTag();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        if (processNdefIntent(intent)) {
            Toast.makeText(this, "Successfully read tag", Toast.LENGTH_SHORT).show();
        } else {
            Toast.makeText(this, "Could not read tag", Toast.LENGTH_SHORT).show();
        }
    }

    private boolean processNdefIntent (Intent intent) {
        idText.setText("");
        noteText.setText("");

        String action = intent.getAction();

        if (NfcAdapter.ACTION_NDEF_DISCOVERED.equals(action)) {
            Ndef ndef = Ndef.get((Tag) intent.getParcelableExtra(NfcAdapter.EXTRA_TAG));
            NdefMessage messages = ndef.getCachedNdefMessage();

            if (messages == null) {
                Log.e(TAG, "Could not get NdefMessages");
                return false;
            }

            NdefRecord[] records = messages.getRecords();
            NdefRecord uriRecord = records[0], noteRecord = records[1];

            Uri uri = uriRecord.toUri();
            if (uri == null) {
                Log.e(TAG, "Invalid tag URI");
                return false;
            }
            String host = uri.getHost();

            if (noteRecord.getTnf() != NdefRecord.TNF_MIME_MEDIA
                    || !("text/plain".equals(new String(noteRecord.getType()))) ) {
                Log.e(TAG, "Could not read payload");
                return false;
            }

            String payload = new String(noteRecord.getPayload(), Charset.forName("US-ASCII"));

            idText.setText(host);
            noteText.setText(payload);

            Log.i(TAG, "Successfully read tag");

            return true;
        }

        return false;
    }

    private IntentFilter buildIntentFilters() {
        IntentFilter filter = new IntentFilter();
        filter.addAction(NfcAdapter.ACTION_NDEF_DISCOVERED);
        filter.addCategory("android.intent.category.DEFAULT");
        filter.addDataScheme("inttag");

        return filter;
    }

    private void enableWaitForTag() {
        Log.d(TAG, "enabling read mode...");
        NfcAdapter adapter = NfcAdapter.getDefaultAdapter(this);
        Intent intent = new Intent(this, getClass());
        intent.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
        IntentFilter[] filters = new IntentFilter[] { buildIntentFilters() };

        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, intent, 0);
        adapter.enableForegroundDispatch(this, pendingIntent, filters, null);
    }

    private void disableWaitForTag() {
        Log.d(TAG, "disabling read mode...");
        NfcAdapter adapter = NfcAdapter.getDefaultAdapter(this);
        adapter.disableForegroundDispatch(this);
    }
}