package com.yoco.commons.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.stereotype.Component;
import org.springframework.util.SerializationUtils;
import javax.annotation.PostConstruct;
import java.util.concurrent.TimeUnit;

@Slf4j
@Lazy(false)
@Component
public class MemoryStoreUtil {

    @Autowired
    RedisTemplate<Object,Object> redisTemplate;

    private static RedisTemplate<Object,Object> template = null;

    private static HashOperations<Object,Object,Object> hashOperations = null;

    public static final long DEFAULT_KEY_EXPIRYTIME_DAYS = 14;

    @PostConstruct
    public void init(){
        if(template == null){
            setRedisTemplate(redisTemplate);
            initializeRedisTemplate(template);
        }
    }

    private static void initializeRedisTemplate(RedisTemplate<Object,Object> redisTemplate){
        var stringSerializer = new StringRedisSerializer();
        redisTemplate.setKeySerializer(stringSerializer);
        redisTemplate.setValueSerializer(stringSerializer);
        redisTemplate.setHashKeySerializer(stringSerializer);
        redisTemplate.afterPropertiesSet();
    }

    public HashOperations<Object,Object,Object> getHashOperationsInstance(){
        if(hashOperations == null){
            setHashOperations(template.opsForHash());
        }
        return hashOperations;
    }

    private static void setHashOperations(HashOperations<Object,Object,Object> hashOperations){
        MemoryStoreUtil.hashOperations = hashOperations;
    }

    private static void setRedisTemplate(RedisTemplate<Object,Object> template){
        MemoryStoreUtil.template = template;
    }

    public void put(String key, String hashKey, Object value){
        if(ObjUtils.isNullOrEmpty(key) || ObjUtils.isNullOrEmpty(hashKey) || ObjUtils.isNull(value)){
            return;
        }
        this.getHashOperationsInstance().put(key,hashKey, SerializationUtils.serialize(value));
    }

    public Object get(String key, String hashKey){
        if(ObjUtils.isNullOrEmpty(key) || ObjUtils.isNullOrEmpty(hashKey)){
            return null;
        }
       return this.getHashOperationsInstance().get(key,hashKey);
    }

    public boolean hasKey(String key, String hashKey){
        if(ObjUtils.isNullOrEmpty(key) || ObjUtils.isNullOrEmpty(hashKey)){
            return false;
        }
        return this.getHashOperationsInstance().hasKey(key,hashKey);
    }

    public void deleteKey(String key){
        if(ObjUtils.isNullOrEmpty(key)){
            return;
        }
        template.delete(key);
    }

    public void deleteHashKey(String key, String hashKey){
        if(ObjUtils.isNullOrEmpty(key) || ObjUtils.isNullOrEmpty(hashKey)){
            return;
        }
        this.getHashOperationsInstance().delete(key,hashKey);
    }

    public void setExpiryInMillis(String key,long expiryTimeOutMillis){
        if(ObjUtils.isNullOrEmpty(key) || expiryTimeOutMillis < 1){
            return;
        }
        template.expire(key,expiryTimeOutMillis, TimeUnit.MILLISECONDS);
    }

    public void setExpiryInDays(String key,long expiryTimeOutDays){
        if(ObjUtils.isNullOrEmpty(key) || expiryTimeOutDays < 1){
            return;
        }
        template.expire(key,expiryTimeOutDays, TimeUnit.DAYS);
    }

    public Long getKeyExpiryTimeInMillis(String key){
        if(ObjUtils.isNullOrEmpty(key)){
            return 0L;
        }
        return template.getExpire(key);
    }

}
