package org.danielge.nfccard;

import android.app.*;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.nfc.FormatException;
import android.nfc.NdefMessage;
import android.nfc.NdefRecord;
import android.nfc.Tag;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;
import org.danielge.nfcskeleton.NfcUtils;
import org.danielge.nfcskeleton.NfcWriterActivity;

import java.io.IOException;
import java.nio.charset.Charset;

public class CardWriterActivity extends NfcWriterActivity {
    public static final String TAG = CardWriterActivity.class.getSimpleName();

    private EditText nameInput, emailInput, phoneInput, workSchoolInput,
                     positionInput, twitterInput, websiteInput, descInput;

    private static final Charset charset = Charset.forName("US-ASCII");

    private NfcWaitDialogFragment waitDialog = null;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.writer);

        nameInput       = (EditText) findViewById(R.id.name_input);
        emailInput      = (EditText) findViewById(R.id.email_input);
        phoneInput      = (EditText) findViewById(R.id.phone_input);
        workSchoolInput = (EditText) findViewById(R.id.work_school_input);
        positionInput   = (EditText) findViewById(R.id.position_major_input);
        twitterInput    = (EditText) findViewById(R.id.twitter_input);
        websiteInput    = (EditText) findViewById(R.id.website_input);
        descInput       = (EditText) findViewById(R.id.desc_input);

    }

    @Override
    protected void onResume() {
        super.onResume();

        Intent intent = getIntent();
        if (Intent.ACTION_VIEW.equals(intent.getAction())) {
            Bundle extras = intent.getExtras();
            for (int etId : CardUtils.getWriterViews()) {
                EditText editText = (EditText) findViewById(etId);

                editText.setText(extras.getString(CardUtils.encode(editText), ""));
            }
        }
    }

    @Override
    protected void onTagDiscovered(Tag tag) {
        CardWriteTask task = new CardWriteTask(this, prepareNdefMessage());
        task.execute(tag);
    }

    private NdefMessage prepareNdefMessage() {
        NdefRecord fieldsRecord = new NdefRecord(
                NdefRecord.TNF_MIME_MEDIA,
                CardUtils.MIME.getBytes(charset),
                new byte[0],
                stringifyInputs().getBytes(charset)
        );

        NdefRecord descRecord = new NdefRecord(
                NdefRecord.TNF_MIME_MEDIA,
                "text/plain".getBytes(charset),
                new byte[0],
                descInput.getText().toString().getBytes(charset)
        );

        NdefRecord metaRecord = new NdefRecord(
                NdefRecord.TNF_MIME_MEDIA,
                "text/plain".getBytes(charset),
                new byte[0],
                CardUtils.META.getBytes(charset)
        );

        return new NdefMessage(new NdefRecord[] { fieldsRecord , descRecord, metaRecord });
    }

    private String stringifyInputs() {
        String encoding = "";
        EditText[] inputs = { nameInput, emailInput, phoneInput, workSchoolInput,
                              positionInput, twitterInput, websiteInput };

        for (EditText input : inputs) {
            encoding += String.format("%s=%s", CardUtils.encode(input),
                    input.getText().toString()) + CardUtils.SEPARATOR;
        }

        return encoding;
    }

    public void writeTagAction (View v) {
        waitDialog = NfcWaitDialogFragment.newInstance(this);
        waitDialog.show(prepareDialog(), "dialog");
        enableWriteTagMode();
    }

    private class CardWriteTask extends AsyncTask<Tag, Void, Boolean> {
        private Context ctx;
        private NdefMessage message;

        private String error = null;

        public CardWriteTask (Context ctx, NdefMessage message) {
            this.ctx = ctx;
            this.message = message;
        }

        @Override
        protected void onPreExecute() {
            waitDialog.dismiss();
            waitDialog = null;
        }

        @Override
        protected Boolean doInBackground(Tag... tags) {
            try {
                return NfcUtils.writeNdefTag(this.message, tags[0]);
            } catch (IOException e) {
                Log.e(TAG, "Could not write to tag", e);
                error = "Could not write to tag. Please try again.";
                return false;
            } catch (FormatException e) {
                Log.e(TAG, "Could not format NDEF message", e);
                error = "Could not write to tag. Please try again.";
                return false;
            } catch (NfcUtils.TagNotWritableException e) {
                Log.e(TAG, "Tag is not writable");
                error = "This tag is not writable. Try another tag.";
                return false;
            } catch (NfcUtils.NdefMessageTooLongException e) {
                Log.e(TAG, "Input was excessive for tag");
                error = "Business card data is too long. Try shortening your inputs.";
                return false;
            }
        }

        @Override
        protected void onPostExecute(Boolean success) {
            if (success) {
                Log.i(TAG, "Successfully wrote card to tag");
                Toast.makeText(ctx, "Successfully wrote to tag", Toast.LENGTH_LONG).show();
            } else {
                Toast.makeText(ctx, error, Toast.LENGTH_LONG).show();
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
            super.onDismiss(dialog);
            activity.disableWriteTagMode();
        }
    }
}