package com.yoco.commons.dataservices.objectify;

import com.google.cloud.datastore.Datastore;
import com.googlecode.objectify.ObjectifyFactory;
import com.googlecode.objectify.ObjectifyService;
import com.googlecode.objectify.util.Closeable;
import com.yoco.commons.entity.*;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;
import static com.googlecode.objectify.ObjectifyService.factory;

public class ObjectifyExtension implements BeforeEachCallback, AfterEachCallback {

    private static final Namespace NAMESPACE = Namespace.create(ObjectifyExtension.class);

    @Override
    public void beforeEach(final ExtensionContext context) {
        final Datastore datastore = LocalDatastoreExtension.getHelper(context).getOptions().getService();

        ObjectifyService.init(new ObjectifyFactory(datastore));

        final Closeable rootService = ObjectifyService.begin();

        context.getStore(NAMESPACE).put(Closeable.class, rootService);

        factory().register(Contact.class);
        factory().register(ProjectJDO.class);
        factory().register(ProjectPeopleJDO.class);
        factory().register(IntegrationsJDO.class);
        factory().register(FcmJDO.class);
        factory().register(SettingsJDO.class);
        factory().register(Client.class);
        factory().register(TaskJDO.class);
        factory().register(UserClockinSubStatusJDO.class);
        factory().register(PeopleRelationJDO.class);
    }

    @Override
    public void afterEach(final ExtensionContext context) {
        final Closeable rootService = context.getStore(NAMESPACE).get(Closeable.class, Closeable.class);
        rootService.close();
    }
}