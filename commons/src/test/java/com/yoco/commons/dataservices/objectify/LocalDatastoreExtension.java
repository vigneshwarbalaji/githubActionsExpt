package com.yoco.commons.dataservices.objectify;

import com.google.cloud.datastore.testing.LocalDatastoreHelper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;

@Slf4j
public class LocalDatastoreExtension implements BeforeAllCallback {

    @Override
    public void beforeAll(final ExtensionContext context) throws Exception {
        if (getHelper(context) == null) {
            final LocalDatastoreHelper helper = LocalDatastoreHelper.create(1.0,7070);
            context.getRoot().getStore(Namespace.GLOBAL).put(LocalDatastoreHelper.class, helper);
            helper.start();
        }
    }

    public static LocalDatastoreHelper getHelper(final ExtensionContext context) {
        return context.getRoot().getStore(Namespace.GLOBAL).get(LocalDatastoreHelper.class, LocalDatastoreHelper.class);
    }
}