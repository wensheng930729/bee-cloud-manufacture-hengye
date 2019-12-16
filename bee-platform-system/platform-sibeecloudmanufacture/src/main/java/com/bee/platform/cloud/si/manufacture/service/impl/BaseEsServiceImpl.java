package com.bee.platform.cloud.si.manufacture.service.impl;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.action.index.IndexRequestBuilder;
import org.elasticsearch.action.search.SearchRequestBuilder;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.RangeQueryBuilder;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.SearchHits;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.annotations.Document;

import com.alibaba.fastjson.JSON;
import com.bee.platform.cloud.si.manufacture.service.EsGenericService;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * <T, E> T为es实体， E为基础实体
 * @author Raphael.dq
 * @date 2019/03/20
 */
@Slf4j
public class BaseEsServiceImpl<T, E> implements EsGenericService<T>{
    
    @Autowired
    protected TransportClient esClient;
    
    private Document docAnnotation;
    
    @Override
    public SearchRequestBuilder search() {
        Document doc = getDocAnnotation();
        System.out.println("indexName : "+doc.indexName()+" type:: "+doc.type());
        return esClient.prepareSearch(doc.type());
    }

    @Override
    public IndexRequestBuilder index() {
        Document doc = getDocAnnotation();
        return esClient.prepareIndex(doc.indexName(), doc.type());
    }

    private Document getDocAnnotation() {
        if (null == docAnnotation) {
            Type[] types = getActualTypes();
            Document doc = ((Class<?>)types[0]).getAnnotation(Document.class);
            if (doc == null) {
                throw new IllegalStateException("inner state error, maybe missing @Document");
            }
            docAnnotation = doc;
        }
         return docAnnotation;
    }
    
    private Type[] getActualTypes() {
        ParameterizedType type = (ParameterizedType)this.getClass().getGenericSuperclass();
         if (type != null) {
             Type[] types = type.getActualTypeArguments();
             if (types.length > 0) {
                 return types;
             } else {
                 throw new IllegalStateException("inner state error, get ActualTypeArguments error");
             }
         } else {
             throw new IllegalStateException("inner state error, get ParameterizedType error");
         }
    }
    
    protected List<E> getPageDataWithFactoryAndDeviceByRange(EsCommonCondition innerCommonCondition) {
        SearchRequestBuilder sqb = search();
        sqb.setQuery(buildRangeConditionWithFactoryAndDevice(innerCommonCondition));
        if (innerCommonCondition.size == null) {
            innerCommonCondition.size = getTotalCount(innerCommonCondition);
        }
        sqb.setSize(innerCommonCondition.size);
        if (innerCommonCondition.pageFlag) {
            // 分页
            Integer pageSize = innerCommonCondition.pageSize;
            Integer page = innerCommonCondition.page;
            Integer currentPage = (page - 1) * pageSize;
            sqb.setFrom((page == null || page <= 0) ? 0 : currentPage).setSize(pageSize);
        }
        // 排序
        if (StringUtils.isNotBlank(innerCommonCondition.sortClause)) {
            String[] strs = innerCommonCondition.sortClause.split("\\.");
            if (strs.length == 2) {
                sqb.addSort(strs[0], strs[1].equalsIgnoreCase("ASC") ? SortOrder.ASC : SortOrder.DESC);
            } else {
                throw new IllegalArgumentException("sortClause 参数错误");
            }
        } else {
            sqb.addSort(innerCommonCondition.timeField, SortOrder.DESC);
        }
        SearchResponse response = sqb.get();
        // 发送get请求提交数据
        SearchHits hits = response.getHits();
        Iterator<SearchHit> iterator = hits.iterator();
        List<E> dataList = new ArrayList<E>();
        while (iterator.hasNext()) {
            SearchHit searchHit1 = iterator.next(); // 每个查询对象
            try {
                E zAirMeterDataDay = (E)JSON.parseObject(searchHit1.getSourceAsString(), getActualTypes()[1]);
                dataList.add(zAirMeterDataDay);
            } catch (Exception e) {
                log.error("数据转换错误", e);
            }
        }
        return dataList;
    }
    
    protected BoolQueryBuilder buildBoolQuery(Map<String, Object> paramMap, boolean isAnd) {
        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();
        for (Map.Entry<String, Object> entry : paramMap.entrySet()) {
            String key = entry.getKey();
            Object val = entry.getValue();
            if (isAnd) {
                boolQuery.must(QueryBuilders.termQuery(key, val));
            } else {
                boolQuery.should(QueryBuilders.termQuery(key, val));
            }
        }
        return boolQuery;
    }
    
    private int getTotalCount(EsCommonCondition innerCommonCondition) {
        SearchRequestBuilder sqb5 = search();
        BoolQueryBuilder qb5 = buildRangeConditionWithFactoryAndDevice(innerCommonCondition);
        sqb5.setQuery(qb5);

        SearchResponse response5 = sqb5.setSize(0).get();
        // 发送get请求提交数据
        SearchHits hits5 = response5.getHits();
        int num = (int)hits5.getTotalHits();
        return num;
    }

    private BoolQueryBuilder buildRangeConditionWithFactoryAndDevice(EsCommonCondition innerCommonCondition) {
        Map<String, Object> paramMap = new HashMap<String, Object>();
        paramMap.put("factoryId", innerCommonCondition.factoryId);
        paramMap.put("deviceId", innerCommonCondition.deviceId);
        BoolQueryBuilder qb5 = buildBoolQuery(paramMap, true);
        // 时间区间
        RangeQueryBuilder rangeQueryBuilder5 = QueryBuilders.rangeQuery(innerCommonCondition.timeField)
            .from(innerCommonCondition.startTime).to(innerCommonCondition.endTime);
        // 条件查询
        qb5.filter(rangeQueryBuilder5);
        return qb5;
    }
    
    @Getter
    @Setter
    @Builder
    public static class EsCommonCondition {
        /**
         * 工厂id
         */
        private Integer factoryId;
        /**
         * 设备id
         */
        private String deviceId;
        /**
         * 获取记录数量
         */
        private Integer size;
        /**
         * 排序段, eg: "time.desc", 则为按time字段降序
         */
        private String sortClause;
        /**
         * 时序字段名称
         */
        private String timeField;
        /**
         * 起始时间
         */
        private Long startTime;
        /**
         * 截止时间
         */
        private Long endTime;
        
        /**
         * 是否只获取当页数据
         */
        private boolean pageFlag;
        
        /**
         * 当前页码
         */
        private Integer page;
        /**
         * 每页大小
         */
        private Integer pageSize;
    }
}
