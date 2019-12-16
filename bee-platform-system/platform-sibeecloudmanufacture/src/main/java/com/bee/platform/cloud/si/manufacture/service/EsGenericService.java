package com.bee.platform.cloud.si.manufacture.service;

import org.elasticsearch.action.index.IndexRequestBuilder;
import org.elasticsearch.action.search.SearchRequestBuilder;

public interface EsGenericService<T> {
     
     SearchRequestBuilder search();
     IndexRequestBuilder index();

}
