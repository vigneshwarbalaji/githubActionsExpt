package com.yoco.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.SerializationUtils;

import java.lang.reflect.Field;
import java.util.concurrent.TimeUnit;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class MemoryStoreUtilTest {

    @Test
    void init_nullTemplateTest() throws NoSuchFieldException, IllegalAccessException {
        try(MockedStatic<MemoryStoreUtil> memoryStoreUtilMockedStatic = mockStatic(MemoryStoreUtil.class)){
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
            Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
            templateField.setAccessible(true);
            templateField.set(null, redisTemplateMock);
            Mockito.doCallRealMethod().when(memoryStoreUtilMock).init();
            memoryStoreUtilMock.init();
            memoryStoreUtilMockedStatic.verifyNoMoreInteractions();
        }
    }

    @Test
    void init_validTemplateTest() throws NoSuchFieldException, IllegalAccessException {
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            memoryStoreUtilMock.redisTemplate = Mockito.mock(RedisTemplate.class);
            Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
            templateField.setAccessible(true);
            templateField.set(null, null);
            Mockito.doCallRealMethod().when(memoryStoreUtilMock).init();
            memoryStoreUtilMock.init();
            RedisTemplate<Object,Object> initializedTemplate = (RedisTemplate<Object, Object>) templateField.get(null);
            Assertions.assertEquals(memoryStoreUtilMock.redisTemplate, initializedTemplate);
            Mockito.verify(initializedTemplate).setKeySerializer(any(StringRedisSerializer.class));
            Mockito.verify(initializedTemplate).setValueSerializer(any(StringRedisSerializer.class));
            Mockito.verify(initializedTemplate).setHashKeySerializer(any(StringRedisSerializer.class));
            Mockito.verify(initializedTemplate).afterPropertiesSet();
    }

    @Test
    void getHashOperationsInstance_ValidInstance_test() throws NoSuchFieldException, IllegalAccessException {
        Field hashOperationsField = MemoryStoreUtil.class.getDeclaredField("hashOperations");
        hashOperationsField.setAccessible(true);
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        hashOperationsField.set(null, hashOperationsMock);
        Assertions.assertEquals(hashOperationsMock,new MemoryStoreUtil().getHashOperationsInstance());
    }

    @Test
    void getHashOperationsInstance_nullInstance_test() throws NoSuchFieldException, IllegalAccessException {
        Field hashOperationsField = MemoryStoreUtil.class.getDeclaredField("hashOperations");
        hashOperationsField.setAccessible(true);
        hashOperationsField.set(null, null);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        Mockito.when(redisTemplateMock.opsForHash()).thenReturn(hashOperationsMock);
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        templateField.set(null, redisTemplateMock);
        Assertions.assertEquals(hashOperationsMock,new MemoryStoreUtil().getHashOperationsInstance());
        Mockito.verify(redisTemplateMock).opsForHash();
    }

    @Test
    void put_nullKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(eq(null),anyString(),any(Object.class));
        memoryStoreUtilMock.put(null,"123","123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void put_emptyKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(anyString(),anyString(),any(Object.class));
        memoryStoreUtilMock.put("","123","123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void put_nullHashKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(anyString(),eq(null),any(Object.class));
        memoryStoreUtilMock.put("123",null,"123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void put_emptyHashKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(anyString(),anyString(),any(Object.class));
        memoryStoreUtilMock.put("123","","123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void put_nullValue_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(anyString(),anyString(),eq(null));
        memoryStoreUtilMock.put("123","123",null);
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void put_Valid_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).put(anyString(),anyString(),any(Object.class));
        memoryStoreUtilMock.put("123","123","123");
        Mockito.verify(hashOperationsMock).put("123","123", SerializationUtils.serialize("123"));
    }

    @Test
    void get_nullKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertNull(memoryStoreUtilMock.get(null,"123"));
    }

    @Test
    void get_emptyKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertNull(memoryStoreUtilMock.get("","123"));
    }

    @Test
    void get_nullHashKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertNull(memoryStoreUtilMock.get("123",null));
    }

    @Test
    void get_emptyHashKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertNull(memoryStoreUtilMock.get("123",""));
    }

    @Test
    void get_valid_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        Mockito.when(hashOperationsMock.get("123","123")).thenReturn("result");
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).get(anyString(),anyString());
        Assertions.assertEquals("result",memoryStoreUtilMock.get("123","123"));
    }


    @Test
    void hasKey_nullKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertFalse(memoryStoreUtilMock.hasKey(null,"123"));
    }

    @Test
    void hasKey_emptyKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertFalse(memoryStoreUtilMock.hasKey("","123"));
    }

    @Test
    void hasKey_nullHashKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertFalse(memoryStoreUtilMock.hasKey("",null));
    }

    @Test
    void hasKey_emptyHashKey_test(){
        MemoryStoreUtil memoryStoreUtilMock = new MemoryStoreUtil();
        Assertions.assertFalse(memoryStoreUtilMock.hasKey("123",""));
    }

    @Test
    void hasKey_valid_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        Mockito.when(hashOperationsMock.hasKey("123","123")).thenReturn(true);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).hasKey(anyString(),anyString());
        Assertions.assertTrue(memoryStoreUtilMock.hasKey("123","123"));
        Mockito.verify(hashOperationsMock).hasKey("123","123");
    }

    @Test
    void deleteKey_nullKey_test() throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().deleteKey(null);
        Mockito.verifyNoInteractions(redisTemplateMock);
    }

    @Test
    void deleteKey_validKey_test() throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().deleteKey("123");
        Mockito.verify(redisTemplateMock).delete("123");
    }

    @Test
    void deleteHashKey_nullKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).deleteHashKey(eq(null),anyString());
        memoryStoreUtilMock.deleteHashKey(null,"123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void deleteHashKey_emptyKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
        memoryStoreUtilMock.deleteHashKey("","123");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void deleteHashKey_nullHashKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).deleteHashKey(anyString(),eq(null));
        memoryStoreUtilMock.deleteHashKey("123",null);
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void deleteHashKey_emptyHashKey_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
        memoryStoreUtilMock.deleteHashKey("123","");
        Mockito.verifyNoInteractions(hashOperationsMock);
    }

    @Test
    void deleteHashKey_Valid_test(){
        HashOperations<Object,Object,Object> hashOperationsMock = Mockito.mock(HashOperations.class);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.getHashOperationsInstance()).thenReturn(hashOperationsMock);
        Mockito.doCallRealMethod().when(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
        memoryStoreUtilMock.deleteHashKey("123","123");
        Mockito.verify(hashOperationsMock).delete("123","123");
    }

    @ParameterizedTest
    @CsvSource({",1", "'',1", "123,0"})
    void setExpiryInMillis_invalidArguments_test(String key, Long expiry) throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().setExpiryInMillis(key,expiry);
        Mockito.verifyNoInteractions(redisTemplateMock);
    }

    @Test
    void setExpiryInMillis_expiryTimeValid_test() throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().setExpiryInMillis("123",123L);
        Mockito.verify(redisTemplateMock).expire("123",123L, TimeUnit.MILLISECONDS);
    }

    @ParameterizedTest
    @CsvSource({",1","'',1","123,0"})
    void setExpiryInDays_invalidArguments_test(String key, Long expiry) throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().setExpiryInDays(key,expiry);
        Mockito.verifyNoInteractions(redisTemplateMock);
    }

    @Test
    void setExpiryInDays_expiryTimeValid_test() throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        templateField.set(null, redisTemplateMock);
        new MemoryStoreUtil().setExpiryInDays("123",123L);
        Mockito.verify(redisTemplateMock).expire("123",123L, TimeUnit.DAYS);
    }

    @Test
    void getKeyExpiryTimeInMillisNullKey_test(){
        Assertions.assertEquals(0L,new MemoryStoreUtil().getKeyExpiryTimeInMillis(null));
    }

    @Test
    void getKeyExpiryTimeInMillisEmptyKey_test(){
        Assertions.assertEquals(0L,new MemoryStoreUtil().getKeyExpiryTimeInMillis(""));
    }

    @Test
    void getKeyExpiryTimeInMillisValidKey_test() throws NoSuchFieldException, IllegalAccessException {
        Field templateField = MemoryStoreUtil.class.getDeclaredField("template");
        templateField.setAccessible(true);
        RedisTemplate<Object,Object> redisTemplateMock = Mockito.mock(RedisTemplate.class);
        Mockito.when(redisTemplateMock.getExpire("123")).thenReturn(123L);
        templateField.set(null, redisTemplateMock);
        Assertions.assertEquals(123L,new MemoryStoreUtil().getKeyExpiryTimeInMillis("123"));
    }

}
