package com.yoco.commons.enums;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.Set;

class ApiScopeTest {
    @Test
    void getAwAccountCreationServiceTokenScopes_test(){
        Assertions.assertEquals(Set.of("awapis.account.create"),ApiScope.getAwAccountCreationServiceTokenScopes());
    }
}
