package com.yoco.commons.annotation.helper;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.SerializationUtils;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
public class AccessTokenCacheService {

    private AccessTokenCacheService(){}

    private static MemoryStoreUtil memoryStoreUtil = new MemoryStoreUtil();
    public static final String SERVERACCESSTOKEN_NAMESPACE = "serverAccessToken";

    public static void putAccessToken(OauthAccessToken accessToken){
        try{
            if(accessToken == null){
                return;
            }
            var tokenType = accessToken.getType().toString();
            if("user".equalsIgnoreCase(tokenType)){
                putUserAccessToken(accessToken);
            }else if("server".equalsIgnoreCase(tokenType)){
                putServerAccessToken(accessToken);
            }
        }catch (Exception e){
            log.info("Error persisting in memorystore ::" + e.getMessage());
        }
    }

    public static OauthAccessToken getAccessToken(String jwt){
        try{
            if(jwt == null || jwt.trim().length() < 1){
                return null;
            }
            Map<String,Object> jwtPayload = JwtUtil.extractPayloadFromJWT(jwt);
            if(JwtUtil.isTokenTypeUser(jwtPayload)){
                return fetchUserAccessToken(jwt,JwtUtil.extractUserContactIDFromJwt(jwtPayload));
            }else if(JwtUtil.isTokenTypeServer(jwtPayload)){
                return fetchServerAccessToken(jwt);
            }else{
                return null;
            }
        }catch (Exception e){
            log.info("Error fetching from memcache ::" + e.getMessage());
            return null;
        }
    }

    public static OauthAccessToken fetchUserAccessToken(String jwt,String contactID) throws NoSuchAlgorithmException {
        String contactKey = HashUtil.generateSHA256(contactID);
        String jwtKey = HashUtil.generateSHA256(jwt);
        OauthAccessToken accessToken = (OauthAccessToken) SerializationUtils.deserialize((byte[]) memoryStoreUtil.get(contactKey,jwtKey));
        if(accessToken == null){
            log.info("Cache hit failure");
            return null;
        }
        if(accessToken.isExpired()){
            memoryStoreUtil.deleteHashKey(contactKey,jwtKey);
            return null;
        }else{
            log.info("Cache hit successful");
            memoryStoreUtil.setExpiryInDays(contactKey,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
        }
        return accessToken;
    }

    public static OauthAccessToken fetchServerAccessToken(String jwt) throws NoSuchAlgorithmException {
        String accessTokenKey = HashUtil.generateSHA256(jwt);
        OauthAccessToken accessToken = (OauthAccessToken) SerializationUtils.deserialize((byte[]) memoryStoreUtil.get(SERVERACCESSTOKEN_NAMESPACE,accessTokenKey));
        if(accessToken == null){
            return null;
        }
        if(accessToken.isExpired()){
            memoryStoreUtil.deleteHashKey(SERVERACCESSTOKEN_NAMESPACE,accessTokenKey);
            return null;
        }else{
            memoryStoreUtil.setExpiryInDays(SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
        }
        return accessToken;
    }

    public static void putUserAccessToken(OauthAccessToken token) throws NoSuchAlgorithmException {
        if(token == null || ObjUtils.isNullOrEmpty(token.getUserId())){
            return;
        }
        String contactKey = HashUtil.generateSHA256(token.getUserId());
        memoryStoreUtil.put(contactKey,HashUtil.generateSHA256(token.getAccessToken()),token);
        memoryStoreUtil.setExpiryInDays(contactKey,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    public static void putServerAccessToken(OauthAccessToken token) throws NoSuchAlgorithmException {
        if(token == null){
            return;
        }
        String key = HashUtil.generateSHA256(token.getAccessToken());
        memoryStoreUtil.put(SERVERACCESSTOKEN_NAMESPACE,key,token);
        memoryStoreUtil.setExpiryInDays(SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    public static void putServerAccessToken(OauthAccessToken token,String key) throws NoSuchAlgorithmException {
        if(token == null){
            return;
        }
        String hashKey = HashUtil.generateSHA256(key);
        memoryStoreUtil.put(SERVERACCESSTOKEN_NAMESPACE,hashKey,token);
        memoryStoreUtil.setExpiryInDays(SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    public static void clearAccessTokensForUser(OauthAccessToken tokenToExclude, String contactID) throws NoSuchAlgorithmException {
        String contactKey = HashUtil.generateSHA256(contactID);
        memoryStoreUtil.deleteKey(contactKey);
        if(tokenToExclude != null){
            putUserAccessToken(tokenToExclude);
        }
    }

    public static void setMemoryStoreUtil(MemoryStoreUtil memoryStoreUtil){
        AccessTokenCacheService.memoryStoreUtil = memoryStoreUtil;
    }
}
