package com.yoco.commons.utils;

import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.cloudkms.v1.CloudKMS;
import com.google.api.services.cloudkms.v1.CloudKMSScopes;
import com.google.api.services.cloudkms.v1.model.DecryptRequest;
import com.google.api.services.cloudkms.v1.model.DecryptResponse;
import com.google.auth.http.HttpCredentialsAdapter;
import com.google.auth.oauth2.GoogleCredentials;
import com.google.common.io.ByteStreams;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Properties;

@Slf4j
public class KMSUtil {

    public static KMSUtil getKMSUtilInstance(){
        return new KMSUtil();
    }

    public Properties loadCredentials(String fileName, String kmsResourceID, String applicationName) throws IOException{
        InputStream is = KMSUtil.class.getClassLoader().getResourceAsStream(fileName);
        byte[] content = ByteStreams.toByteArray(is);

        String decryptedContent = decrypt(kmsResourceID, content, applicationName);
        var props = new Properties();
        props.load(new StringReader(decryptedContent));
        return props;
    }

    public String decrypt(String kmsResourceID, byte[] content, String applicationName) throws IOException {

        CloudKMS kms = createAuthorizedClient(applicationName);
        DecryptRequest request = new DecryptRequest().encodeCiphertext(content);
        DecryptResponse response = kms.projects().locations().keyRings().cryptoKeys().decrypt(kmsResourceID, request).execute();

        return new String(response.decodePlaintext());
    }

    private CloudKMS createAuthorizedClient(String applicationName) throws IOException {

        HttpTransport transport = new NetHttpTransport();
        JsonFactory jsonFactory = new JacksonFactory();

        var credentials = GoogleCredentials.getApplicationDefault();

        if (credentials.createScopedRequired()) {
            credentials = credentials.createScoped(CloudKMSScopes.all());
        }

        return new CloudKMS.Builder(transport, jsonFactory, new HttpCredentialsAdapter(credentials))
                .setApplicationName(applicationName).build();
    }

}
