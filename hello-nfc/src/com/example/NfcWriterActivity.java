package com.example;

import android.app.*;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.nfc.*;
import android.nfc.tech.Ndef;
import android.nfc.tech.NdefFormatable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;

import java.io.IOException;
import java.nio.charset.Charset;

public class NfcWriterActivity extends Activity
{
    public static final String TAG = NfcWriterActivity.class.getSimpleName();

    private DialogFragment nfcWaitDialog = null;
    private boolean writeMode = false;

    private EditText textInput;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        textInput = (EditText) findViewById(R.id.note_input);
    }

    @Override
    protected void onStop() {
        super.onStop();
        nfcWaitDialog = null;
    }

    @Override
    protected void onNewIntent(Intent intent) {
        String action = intent.getAction();
        Log.d(TAG, "Got intent with action " + action);
        Log.d(TAG, "writeMode is active? " + writeMode);
        if (writeMode && NfcAdapter.ACTION_TAG_DISCOVERED.equals(action)) {
            Log.d(TAG, "Going to write to tag");
            Tag tag = intent.getParcelableExtra(NfcAdapter.EXTRA_TAG);
            NfcWriteTask task = new NfcWriteTask(this, prepareMessage());
            task.execute(tag);
            nfcWaitDialog.dismiss();
        }
    }

    private NdefMessage prepareMessage() {
        int id = (int) (Math.random() * Integer.MAX_VALUE);
        String note = textInput.getText().toString();
        String uri = "inttag://" + id + "/";

        NdefRecord uriRecord = new NdefRecord(
                NdefRecord.TNF_ABSOLUTE_URI,
                uri.getBytes(Charset.forName("US-ASCII")),
                new byte[0], new byte[0]);

        NdefRecord intRecord = new NdefRecord(
                NdefRecord.TNF_MIME_MEDIA,
                "text/plain".getBytes(),
                new byte[0],
                note.getBytes(Charset.forName("US-ASCII")));

        return new NdefMessage(new NdefRecord[] { uriRecord, intRecord });
    }

    private IntentFilter buildIntentFilters() {
        IntentFilter filter = new IntentFilter();
        filter.addAction(NfcAdapter.ACTION_TAG_DISCOVERED);
        filter.addCategory("android.intent.category.DEFAULT");

        return filter;
    }

    private void enableWaitForTag() {
        Log.d(TAG, "enabling write mode...");
        writeMode = true;

        NfcAdapter adapter = NfcAdapter.getDefaultAdapter(this);
        Intent intent = new Intent(this, getClass());
        intent.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
        IntentFilter[] filters = new IntentFilter[] { buildIntentFilters() };

        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, intent, 0);
        adapter.enableForegroundDispatch(this, pendingIntent, filters, null);
    }

    private void disableWaitForTag() {
        Log.d(TAG, "disabling write mode...");
        writeMode = false;
        NfcAdapter adapter = NfcAdapter.getDefaultAdapter(this);
        adapter.disableForegroundDispatch(this);
    }

    public void writeTagAction (View v) {
        nfcWaitDialog = NfcWaitDialogFragment.newInstance(this);
        nfcWaitDialog.show(prepareDialog(), "dialog");
        enableWaitForTag();
    }

    private static class NfcWriteTask extends AsyncTask<Tag, Void, Boolean> {

        private NdefMessage message;
        private Context ctx;

        public NfcWriteTask (Context ctx, NdefMessage message) {
            this.message = message;
            this.ctx = ctx;
        }

        @Override
        protected Boolean doInBackground(Tag... tags) {
            Tag tag = tags[0];
            return writeTag(message, tag);
        }

        private boolean writeTag (NdefMessage message, Tag tag) {
            Log.i(TAG, "Writing to tag...");
            int size = message.toByteArray().length;
            try {
                Ndef ndef = Ndef.get(tag);
                if (ndef != null) {
                    ndef.connect();

                    if (!ndef.isWritable()) {
                        Log.e(TAG, "Tag is not writable");
                        return false;
                    }
                    if (ndef.getMaxSize() < size) {
                        Log.e(TAG, "Message is too long");
                        return false;
                    }

                    ndef.writeNdefMessage(message);
                    return true;
                } else {
                    NdefFormatable format = NdefFormatable.get(tag);
                    if (format != null) {
                        try {
                            format.connect();
                            format.format(message);
                            return true;
                        } catch (IOException e) {
                            return false;
                        }
                    }
                }
            } catch (IOException e) {
                Log.e(TAG, "Could not write to tag (IOException)");
            } catch (FormatException e) {
                Log.e(TAG, "Malformed message");
            }

            return false;
        }

        @Override
        protected void onPostExecute(Boolean success) {
            if (success) {
                Log.i(TAG, "Successfully wrote payload to tag");
                Toast.makeText(ctx, "Successfully wrote to tag", Toast.LENGTH_SHORT).show();
            } else {
                Toast.makeText(ctx, "Failed to write to tag", Toast.LENGTH_SHORT).show();
            }
        }
    }

    private FragmentTransaction prepareDialog() {
        FragmentTransaction ft = getFragmentManager().beginTransaction();
        Fragment prev = getFragmentManager().findFragmentByTag("dialog");
        if (prev != null) {
            ft.remove(prev);
        }
        ft.addToBackStack(null);

        return ft;
    }

    private static class NfcWaitDialogFragment extends DialogFragment {

        private NfcWriterActivity activity;

        private NfcWaitDialogFragment(NfcWriterActivity activity) {
            this.activity = activity;
        }

        public static NfcWaitDialogFragment newInstance (NfcWriterActivity activity) {
            NfcWaitDialogFragment frag = new NfcWaitDialogFragment(activity);
            Bundle args = new Bundle();
            frag.setArguments(args);
            return frag;
        }

        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            AlertDialog.Builder builder;
            builder = new AlertDialog.Builder(getActivity())
                    .setMessage(R.string.nfc_wait_text);

            return builder.create();
        }

        @Override
        public void onDismiss(DialogInterface dialog) {
            activity.disableWaitForTag();
        }
    }
}
