package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class ClientImplTest {

    @Test
    void getClientImplInstance_test(){
        Assertions.assertEquals(ClientImpl.class, ClientImpl.getClientImplInstance().getClass());
    }


}
