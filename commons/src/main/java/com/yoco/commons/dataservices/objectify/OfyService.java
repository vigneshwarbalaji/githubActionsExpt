package com.yoco.commons.dataservices.objectify;

import com.google.api.client.util.Lists;
import com.google.cloud.datastore.Cursor;
import com.google.cloud.datastore.QueryResults;
import com.googlecode.objectify.Key;
import com.googlecode.objectify.Objectify;
import com.googlecode.objectify.ObjectifyService;
import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class OfyService {

    public static Objectify ofy() {
        return ObjectifyService.ofy();
    }

    public <T> T get(Class<T> clazz, String key) {
        return ObjUtils.isNullOrEmpty(key) ? null : ofy().load().type(clazz).id(key).now();
    }

    public <T> T get(Class<T> clazz, long key) {
        return key <= 0 ? null : ofy().load().type(clazz).id(key).now();
    }

    public <T, S> List<T> get(Class<T> clazz, Iterable<S> ids) {
        if (ids == null)
            return new ArrayList<>();
        Map<S, T> result = ofy().load().type(clazz).ids(ids);
        if (result == null)
            return new ArrayList<>();
        return new ArrayList<>(result.values());
    }

    public <T> Key<T> save(T entity) {
        if (entity == null)
            return null;
        return ofy().save().entity(entity).now();
    }

    public <T> void saveCollection(List<T> entities) {
        if (entities != null && !entities.isEmpty())
            ofy().save().entities(entities).now();
    }

    public void delete(Object entity) {
        if (entity == null)
            return;
        ofy().delete().entity(entity).now();
    }

    public void delete(List<Object> entities) {
        if (entities == null || entities.isEmpty())
            return;
        ofy().delete().entities(entities);
    }

    public <T> CollectionResponse<T> fetchCursorQuery(Query<T> query, int limit, String cursor) {

        if (query == null)
            throw new IllegalArgumentException("invalid query");

        try {

            if (cursor != null){
                query = query.startAt(Cursor.fromUrlSafe(cursor));
            }

            query = query.limit(limit);

            QueryResults<T> iterator =  query.iterator();

            List<T> items = Lists.newArrayList(iterator);

            String cursorString = null;

            if(iterator.getCursorAfter() != null){
                cursorString = items.size() < limit ? null : iterator.getCursorAfter().toUrlSafe();
            }

            return CollectionResponse.<T>builder()
                    .setItems(items)
                    .setCursor(cursorString)
                    .build();

        }catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }

}
