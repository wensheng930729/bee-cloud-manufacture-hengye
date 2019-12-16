package com.bee.platform.cloud.user.service.resource.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.user.config.annotation.ResourceTypeAnn;
import com.bee.platform.cloud.user.dto.FunctionListDTO;
import com.bee.platform.cloud.user.entity.AuthResource;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.cloud.user.service.resource.AbstractResourceStrategy;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.constants.enums.ResourceType;
import com.bee.platform.common.enums.Status;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-11-05 09:37
 **/
@Service
@ResourceTypeAnn(platform = PlatformType.CLOUD_MAF_BI)
public class BiResourceServiceImpl extends AbstractResourceStrategy {

    @Autowired
    private AuthResourceService authResourceService;
    /**
     * @notes: 查询Bi相关的所有功能
     * @Author: junyang.li
     * @Date: 9:40 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    @Override
    public List<FunctionListDTO> getFunction() {
        //查询app的所有功能
        List<AuthResource> resource=authResourceService.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and resource_type={0} and sub_sys={1}",
                        ResourceType.FUNCTION.getKey(),PlatformType.CLOUD_MAF_BI.getValue()));
        //判空
        if(CollectionUtils.isEmpty(resource)){
            return new ArrayList<>();
        }
        //遍历
        return resource.stream().map(obj->{
            return new FunctionListDTO()
                    .setResourceId(obj.getId())
                    .setResourceName(obj.getName())
                    .setSelection(Status.FALSE.getKey());
        }).collect(Collectors.toList());
    }
    /**
     * @notes: 获得所有bi端的资源
     * @Author: junyang.li
     * @Date: 9:51 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    @Override
    public List<AuthResource> getResource() {
        //缓存中查询所有的app资源
        return authResourceService.selectAuthResource(PlatformType.CLOUD_MAF_BI);
    }
    /**
     * @notes: 校验工厂bi配置的功能，并返回存在的功能id
     * @Author: junyang.li
     * @Date: 10:12 2019/11/5
     * @param functionIds :
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> checkFunctionIds(List<Integer> functionIds) {
        //获得所有web的资源
        List<AuthResource> list=authResourceService.selectAuthResource(PlatformType.CLOUD_MAF_BI);
        //遍历所有的web资源，将他自己，和父id是他的放入集合中
        List<Integer> all=new ArrayList<>();
        for (AuthResource item:list) {
            Integer resourceId=item.getId();
            Integer pid=item.getPid();
            if(functionIds.contains(resourceId) ||
                    functionIds.contains(pid)){
                all.add(resourceId);
                if(Status.FALSE.getKey().equals(pid)){
                    continue;
                }
                if(!all.contains(pid)){
                    all.add(pid);
                }
            }
        }
        return all;
    }
}
