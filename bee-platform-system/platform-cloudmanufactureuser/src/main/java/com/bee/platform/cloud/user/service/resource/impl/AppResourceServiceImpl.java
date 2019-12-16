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
@ResourceTypeAnn(platform = PlatformType.CLOUD_MAF_APP)
public class AppResourceServiceImpl extends AbstractResourceStrategy {

    @Autowired
    private AuthResourceService authResourceService;
    /**
     * @notes: 查询app相关的所有功能
     * @Author: junyang.li
     * @Date: 9:40 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    @Override
    public List<FunctionListDTO> getFunction() {
        //查询app的所有功能
        List<AuthResource> resource=authResourceService.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and resource_type={0} and sub_sys={1}",
                        ResourceType.FUNCTION.getKey(), PlatformType.CLOUD_MAF_APP.getValue()));
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
     * @notes: 获得所有app的资源
     * @Author: junyang.li
     * @Date: 9:50 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    @Override
    public List<AuthResource> getResource() {
        //缓存中查询所有的app资源
        return authResourceService.selectAuthResource(PlatformType.CLOUD_MAF_APP);
    }
    /**
     * @notes: 校验app的功能，并返回存在的功能id
     * @Author: junyang.li
     * @Date: 10:12 2019/11/5
     * @param functionIds :
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> checkFunctionIds(List<Integer> functionIds) {
        //查询所有的app资源
        List<AuthResource> all=this.getResource();
        //遍历
        return all.stream()
                .filter(obj->functionIds.contains(obj.getId()))
                .map(AuthResource::getId)
                .collect(Collectors.toList());
    }
}
