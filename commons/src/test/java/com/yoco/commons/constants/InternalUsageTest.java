package com.yoco.commons.constants;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class InternalUsageTest {

    @Test
    void isFullAccount_nonFull_test(){
        Assertions.assertFalse(InternalUsage.isFullUsAccount("123"));
    }

    @Test
    void isFullAccount_Full_test(){
        Assertions.assertTrue(InternalUsage.isFullUsAccount(InternalUsage.FULL));
    }
}