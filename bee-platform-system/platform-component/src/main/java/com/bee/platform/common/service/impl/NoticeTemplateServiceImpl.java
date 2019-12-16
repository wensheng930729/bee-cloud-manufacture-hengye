package com.bee.platform.common.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dao.mapper.NoticeTemplateMapper;
import com.bee.platform.common.entity.NoticeTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.NoticeTemplateService;
import com.bee.platform.common.utils.ConstantsUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * 后台管理系统通知模板 服务实现类
 *
 * @author junyang.li123
 * @since 2019-05-06
 */
@Slf4j
@Service
public class NoticeTemplateServiceImpl extends ServiceImpl<NoticeTemplateMapper, NoticeTemplate> implements NoticeTemplateService {

    @Autowired
    private JedisService jedisService;
    /**
     * @notes: 通过模板类型查询模板对象
     * @Author: junyang.li
     * @Date: 16:32 2019/5/6
     * @param templateType : 模板类型
     * @return: com.bee.platform.common.entity.NoticeTemplate
     */
    @Override
    public NoticeTemplate getTemplateByType(NoticeTemplateType templateType) {
        if(templateType==null){
            return null;
        }
        //从缓存中查询模板对象
        try {
            NoticeTemplate template=jedisService.getHash(ConstantsUtil.ALL_NOTICE_TEMPLATE,templateType.getKey(),NoticeTemplate.class);
            if(template!=null){
                return template;
            }
            Map<Integer,NoticeTemplate> map=getAllTemplate();
            if(CollectionUtils.isEmpty(map)){
                return null;
            }
            jedisService.setHash(ConstantsUtil.ALL_NOTICE_TEMPLATE,map);
            return map.get(templateType.getKey());
        }catch (JedisConnectionException e){
            log.error("通过模板类型查询模板对象时数据库连接异常，异常信息是：{}",e);
            Map<Integer,NoticeTemplate> map=getAllTemplate();
            if(CollectionUtils.isEmpty(map)){
                return null;
            }
            return map.get(templateType.getKey());
        }
    }

    /**
     * @notes: 批量查询模板对象
     * @Author: junyang.li
     * @Date: 16:55 2019/5/6
     * @param templateTypes : 模板类型数组
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.NoticeTemplate>
     */
    @Override
    public Map<Integer,NoticeTemplate> getTemplateByTypes(NoticeTemplateType[] templateTypes) {
        Map<Integer,NoticeTemplate> map=new HashMap<>(16);
        //判空
        if(templateTypes==null){
            return map;
        }
        //遍历数组
        for (NoticeTemplateType item:templateTypes) {
            NoticeTemplate template=getTemplateByType(item);
            map.put(template.getTemplateType(),template);
        }
        return map;
    }

    /**
     * @notes: 数据库中查询所有的模板
     * @Author: junyang.li
     * @Date: 16:40 2019/5/6
     * @return: java.util.List<com.bee.platform.common.entity.NoticeTemplate>
     */
    private Map<Integer,NoticeTemplate> getAllTemplate(){
        List<NoticeTemplate> list= this.selectList(new EntityWrapper<NoticeTemplate>().where("status={0}", Status.TRUE.getKey()));
        return list.stream().collect(Collectors.toMap(NoticeTemplate::getTemplateType,noticeTemplate -> noticeTemplate));
    }
}
