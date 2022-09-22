package com.yoco.commons.fullservices.cache;

import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.MemoryStoreUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;

import static org.mockito.ArgumentMatchers.*;

class FullDataCacheImplTest {

    FullDataCacheImpl fullDataCache = new FullDataCacheImpl();

    @Test
    void get_test(){
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(null);
        FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertNull(fullDataCache.get("key"));
    }

    @Test
    void get_Exception_test(){
        try(MockedStatic<HashUtil> hashUtilMockedStatic = Mockito.mockStatic(HashUtil.class)){
            hashUtilMockedStatic.when(()-> HashUtil.generateSHA256(anyString())).thenThrow(new NoSuchAlgorithmException());
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(null);
            FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
            Assertions.assertNull(fullDataCache.get("key"));
            hashUtilMockedStatic.verify(()-> HashUtil.generateSHA256("key"));
        }
    }

    @Test
    void put_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.doNothing().when(memoryStoreUtilMock).put(anyString(),anyString(),any());
        Mockito.doNothing().when(memoryStoreUtilMock).setExpiryInDays(anyString(),anyLong());
        FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
        FullDataCacheImpl.setNameSpace("nameSpace");
        fullDataCache.put("key",new ArrayList<>(),0);
        Mockito.verify(memoryStoreUtilMock).put("nameSpace",HashUtil.generateSHA256("key"),new ArrayList<>());
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays("nameSpace",MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    @Test
    void put_Exception_test(){
        try(MockedStatic<HashUtil> hashUtilMockedStatic = Mockito.mockStatic(HashUtil.class)){
            hashUtilMockedStatic.when(()-> HashUtil.generateSHA256(anyString())).thenThrow(new NoSuchAlgorithmException());
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            Mockito.doNothing().when(memoryStoreUtilMock).put(anyString(),anyString(),any());
            Mockito.doNothing().when(memoryStoreUtilMock).setExpiryInDays(anyString(),anyLong());
            FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
            FullDataCacheImpl.setNameSpace("nameSpace");
            fullDataCache.put("key",new ArrayList<>(),0);
            hashUtilMockedStatic.verify(()-> HashUtil.generateSHA256("key"));
        }
    }

    @Test
    void remove_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.doNothing().when(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
        FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
        FullDataCacheImpl.setNameSpace("nameSpace");
        fullDataCache.remove("key");
        Mockito.verify(memoryStoreUtilMock).deleteHashKey("nameSpace",HashUtil.generateSHA256("key"));
    }

    @Test
    void remove_Exception_test(){
        try(MockedStatic<HashUtil> hashUtilMockedStatic = Mockito.mockStatic(HashUtil.class)){
            hashUtilMockedStatic.when(()-> HashUtil.generateSHA256(anyString())).thenThrow(new NoSuchAlgorithmException());
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            Mockito.doNothing().when(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
            FullDataCacheImpl.setMemoryStoreUtil(memoryStoreUtilMock);
            FullDataCacheImpl.setNameSpace("nameSpace");
            fullDataCache.remove("key");
            hashUtilMockedStatic.verify(()-> HashUtil.generateSHA256("key"));
        }
    }

}