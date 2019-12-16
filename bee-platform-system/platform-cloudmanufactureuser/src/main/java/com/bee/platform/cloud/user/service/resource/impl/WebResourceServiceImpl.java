package com.bee.platform.cloud.user.service.resource.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.user.config.annotation.ResourceTypeAnn;
import com.bee.platform.cloud.user.dto.FunctionListDTO;
import com.bee.platform.cloud.user.dto.SimpleResourceDTO;
import com.bee.platform.cloud.user.entity.AuthResource;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.cloud.user.service.resource.AbstractResourceStrategy;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.constants.enums.ResourceType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.ConstantsUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-11-05 09:37
 **/
@Service
@ResourceTypeAnn(platform = PlatformType.CLOUD_MAF_WEB)
public class WebResourceServiceImpl extends AbstractResourceStrategy {

    @Autowired
    private AuthResourceService authResourceService;

    /**
     * @notes: 查询工厂配置web端相关的所有功能
     * @Author: junyang.li
     * @Date: 9:40 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    @Override
    public List<FunctionListDTO> getFunction() {
        //查询web端所有的菜单
        List<AuthResource> resource=authResourceService.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and resource_type={0} and sub_sys={1}",
                        ResourceType.FUNCTION.getKey(),PlatformType.CLOUD_MAF_WEB.getValue()));
        return resource.stream().map(obj->{
            return new FunctionListDTO(obj.getId(),obj.getName(),Status.FALSE.getKey());
        }).collect(Collectors.toList());
    }
    /**
     * @notes: 获得所有工厂配置web端的资源
     * @Author: junyang.li
     * @Date: 9:51 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    @Override
    public List<AuthResource> getResource() {
        //缓存中查询所有的app资源
        return authResourceService.selectAuthResource(PlatformType.CLOUD_MAF_WEB);
    }
    /**
     * @notes: 校验工厂web配置的功能，并返回存在的功能id
     * @Author: junyang.li
     * @Date: 10:12 2019/11/5
     * @param functionIds :
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> checkFunctionIds(List<Integer> functionIds) {
        //获得所有web的资源
        List<AuthResource> list=this.getResource();
        //遍历所有的web资源，将他自己，和父id是他的放入集合中
        return list.stream()
                .filter(obj-> functionIds.contains(obj.getId()) || functionIds.contains(obj.getPid()))
                .map(AuthResource::getId)
                .collect(Collectors.toList());
    }
    /**
     * @notes: 获取web端所有的功能点，遍历成树
     * @Author: junyang.li
     * @Date: 14:43 2019/9/26
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    public List<SimpleResourceDTO> getWebAllFunction(){
        //查询web端所有的菜单
        List<AuthResource> resource=authResourceService.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and resource_type={0} and sub_sys={1}",
                        ResourceType.FUNCTION.getKey(),PlatformType.CLOUD_MAF_WEB.getValue()));
        return this.getWebAllFunction(resource);
    }
    /**
     * @notes: 获取web端所有的功能点，遍历成树
     * @Author: junyang.li
     * @Date: 14:43 2019/9/26
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    public List<SimpleResourceDTO> getWebAllFunction(List<AuthResource> resource){
        //判空
        if(CollectionUtils.isEmpty(resource)){
            return new ArrayList<>();
        }
        Map<Integer,SimpleResourceDTO> map=new HashMap<>(16);
        //遍历成目标对象
        List<SimpleResourceDTO> list=resource.stream().map(obj->{
            SimpleResourceDTO dto=new SimpleResourceDTO(obj.getId(),obj.getName(),obj.getPid(),
                    Status.FALSE.getKey());
            map.put(obj.getId(),dto);
            return dto;
        }).collect(Collectors.toList());

        return this.getWebAllFunction(list,map);
    }
    /**
     * @notes: 获取web端所有的功能点，遍历成树
     * @Author: junyang.li
     * @Date: 14:43 2019/9/26
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    public List<SimpleResourceDTO> getWebAllFunction(List<SimpleResourceDTO> list,Map<Integer,SimpleResourceDTO> map){
        //返回对象
        List<SimpleResourceDTO> resources=new ArrayList<>();
        //遍历成树
        list.forEach(obj->{
            if(ConstantsUtil.ZERO == obj.getPid()){
                resources.add(obj);
            }else {
                SimpleResourceDTO parent = map.get(obj.getPid());
                if(CollectionUtils.isEmpty(parent.getChildren())){
                    parent.setChildren(new ArrayList<>());
                }
                parent.getChildren().add(obj);
            }
        });
        return resources;
    }
}
