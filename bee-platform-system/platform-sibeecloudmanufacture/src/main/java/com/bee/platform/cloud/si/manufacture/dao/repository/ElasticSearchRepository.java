package com.bee.platform.cloud.si.manufacture.dao.repository;



import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.IndexQuery;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

/**
 * @ClassName: ElasticSearchRepository
 * @Description: elasticSearch 操作公共类
 * @Author: fei.sun
 * @Date: 2019/10/25 16:52
 * @Version: 1.0
 */
@Slf4j
@Component
public class  ElasticSearchRepository<T> {

    private ElasticsearchTemplate elasticsearchTemplate;

    @Autowired
    public ElasticSearchRepository(ElasticsearchTemplate elasticsearchTemplate){
        this.elasticsearchTemplate = elasticsearchTemplate;
    }


    public boolean insert(Class<T> clazz, T data){
        if(ObjectUtils.isEmpty(data)){
            return false;
        }
        if(!elasticsearchTemplate.indexExists(clazz)){
            elasticsearchTemplate.createIndex(clazz);
            elasticsearchTemplate.putMapping(clazz);
            log.info("创建了电表数据索引！");
        }
        IndexQuery indexQuery = new IndexQuery();
        indexQuery.setObject(data);
        String index = elasticsearchTemplate.index(indexQuery);
        System.out.println("插入数据成功后，es返回的数据是 ： "+index);
        return true;
    }
}
