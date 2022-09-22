package com.yoco.commons.entity;

import com.yoco.commons.enums.Status;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ClientTest {

    @Test
    void client_valid_test() throws Exception {
        Client client= new Client("cliName","mail@domain","111111","A","accountId","requester");
        assertEquals(Status.ACTIVE.toString(),client.getStatus());
        assertFalse(ObjUtils.isNullOrEmpty(client.getId()));
    }
}
