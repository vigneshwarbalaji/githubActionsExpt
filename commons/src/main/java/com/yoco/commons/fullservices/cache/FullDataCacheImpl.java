package com.yoco.commons.fullservices.cache;

import com.fullauth.api.manage.cache.DataCacher;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.MemoryStoreUtil;
import lombok.extern.slf4j.Slf4j;

import java.security.NoSuchAlgorithmException;

@Slf4j
public class FullDataCacheImpl implements DataCacher {

    private static MemoryStoreUtil memoryStoreUtil = new MemoryStoreUtil();
    private static String nameSpace = "";

    public static void setNameSpace(String nameSpaceKey){
        FullDataCacheImpl.nameSpace = nameSpaceKey;
    }

    public static void setMemoryStoreUtil(MemoryStoreUtil memoryStoreUtil){
        FullDataCacheImpl.memoryStoreUtil = memoryStoreUtil;
    }

    @Override
    public Object get(String key) {
        try {
            return memoryStoreUtil.get(nameSpace,HashUtil.generateSHA256(key));
        } catch (NoSuchAlgorithmException e) {
            log.info(" exception while fetching the key from cache... "+ e.getMessage());
           return null;
        }
    }

    @Override
    public void put(String key, Object data, int expiry) {
        try {
            memoryStoreUtil.put(nameSpace, HashUtil.generateSHA256(key), data);
            memoryStoreUtil.setExpiryInDays(nameSpace, MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
        } catch (NoSuchAlgorithmException e) {
           log.info("exception while persisting to cache... "+ e.getMessage());
        }
    }

    @Override
    public void remove(String key) {
        try {
            memoryStoreUtil.deleteHashKey(nameSpace,HashUtil.generateSHA256(key));
        } catch (NoSuchAlgorithmException e) {
            log.info("exception while removing from cache... " + e.getMessage());
        }
    }
}
