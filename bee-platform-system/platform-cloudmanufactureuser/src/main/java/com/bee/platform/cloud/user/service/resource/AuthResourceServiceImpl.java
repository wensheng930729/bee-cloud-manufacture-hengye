package com.bee.platform.cloud.user.service.resource;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthResourceMapper;
import com.bee.platform.cloud.user.entity.AuthResource;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.constants.enums.ResourceType;
import com.bee.platform.common.entity.AuthResourceInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import redis.clients.jedis.exceptions.JedisException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @description:  资源相关的公共方法
 * @author: junyang.li
 * @create: 2019-11-06 09:26
 **/
@Slf4j
@Service
public class AuthResourceServiceImpl extends ServiceImpl<AuthResourceMapper, AuthResource> implements AuthResourceService {

    @Autowired
    protected AuthResourceMapper authResourceMapper;

    @Autowired
    protected JedisService jedisService;
    /**
     * @notes: 获得所有的功能id
     * @Author: junyang.li
     * @Date: 14:50 2019/11/5
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> getAllFunctionIds(){
        //查询app的所有功能
        List<AuthResource> resource=this.authResourceMapper.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and resource_type={0} ", ResourceType.FUNCTION.getKey()));
        if(CollectionUtils.isEmpty(resource)){
            return new ArrayList<>();
        }
        return resource.stream().map(AuthResource::getId).collect(Collectors.toList());
    }
    /**
     * @notes: 查询指子应用的所有资源
     * @Author: junyang.li
     * @Date: 16:43 2019/9/26
     * @param platform : 应用标识
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    @Override
    public List<AuthResource> selectAuthResource(PlatformType platform){
        String key= ConstantsUtil.CMF+platform.getValue();
        try {
            jedisService.delKey(key);
            String value=jedisService.get(key);
            if(value==null){
                //从数据库中查询
                List<AuthResource> list=this.selectList(new EntityWrapper<AuthResource>()
                        .where("deleted=0 and sub_sys={0}",platform.getValue()));
                //存入缓存中
                jedisService.set(key, JSON.toJSONString(list),ConstantsUtil.OVERDUE);
                return list;
            }
            return JSONArray.parseArray(value,AuthResource.class);
        }catch (JedisException e){
            log.error("从缓存中获取所有app端的资源失败，异常信息是：{}",e);
        }
        return this.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0 and sub_sys={0}",platform.getValue()));
    }


    /**
     * @notes: 通过角色id查询对应的功能
     * @Author: junyang.li
     * @Date: 14:33 2019/9/24
     * @param roleId : 角色id
     * @return: com.bee.platform.cloud.user.dto.FunctionDTO
     */
    @Override
    public List<AuthResourceInfo> getResourceByRoleId(Integer roleId) {
        List<AuthResource> resources=this.selectResourceByRoleId(roleId,null);
        return this.getResourceToTree(resources);
    }
    /**
     * @notes: 通过角色id查询对应的功能
     * @Author: junyang.li
     * @Date: 10:06 2019/9/25
     * @param roleId : 角色id
     * @param cloudMafType : 子应用id
     * @return: java.util.List<com.bee.platform.common.entity.AuthResourceInfo>
     */
    @Override
    public List<AuthResourceInfo> getResourceByRoleId(Integer roleId, String cloudMafType) {
        //判空
        if(roleId==null || StringUtils.isEmpty(cloudMafType)){
            return new ArrayList<>();
        }
        //从缓存中获得
        String key=ConstantsUtil.CMF_RESOURCE_ROLE_HASH_KEY+roleId;
        List<AuthResource> resources;
        try {
            jedisService.delKey(key);
            //从缓存中获取
            String value=jedisService.getHashToJsonStr(key,cloudMafType);
            if(value==null){
                //从数据库中获取
                resources=this.selectResourceByRoleId(roleId,cloudMafType);
                if(CollectionUtils.isEmpty(resources)){
                    return new ArrayList<>();
                }
                //不为空，存入缓存中
                Map<String,String> map=new HashMap<>(1);
                map.put(cloudMafType, JSONArray.toJSONString(resources));
                jedisService.setHash(key,map);
                return this.getResourceToTree(resources);
            }
            resources=JSONArray.parseArray(value,AuthResource.class);
            return this.getResourceToTree(resources);
        }catch (JedisException e){
            log.error("角色查询资源时缓存异常，键是：{}，异常信息是：{}",key,e);
            //查询角色对应的所有资源
            List<AuthResource> resourceIds=authResourceMapper.getResourceByRoleId(roleId,cloudMafType);
            //判空
            if(CollectionUtils.isEmpty(resourceIds)){
                return new ArrayList<>();
            }
            //通过资源id查询资源详细
            return this.getResourceToTree(resourceIds);
        }
    }

    /**
     * @notes: 从数据中查询 角色对应子应用的资源
     * @Author: junyang.li
     * @Date: 13:44 2019/9/25
     * @param roleId : 角色
     * @param cloudMafType : 子应用标识
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    @Override
    public List<AuthResource> selectResourceByRoleId(Integer roleId, String cloudMafType){
        //查询角色对应的所有资源
        List<AuthResource> list= authResourceMapper.getResourceByRoleId(roleId,cloudMafType);
        //再次查询默认资源菜单
        List<AuthResource> def=authResourceMapper.selectList(new EntityWrapper<AuthResource>()
                .where("deleted =0 and sub_sys={0} and resource_type={1}",cloudMafType,ResourceType.DEFAULT.getKey()));
        if(!CollectionUtils.isEmpty(def)){
            list.addAll(def);
        }
        return list;
    }
    /**
     * @notes:  获得所有菜单资源，并遍历成树
     * @Author: junyang.li
     * @Date: 16:55 2019/9/24
     * @return: com.bee.platform.common.entity.ResourceDTO
     */
    @Override
    public List<AuthResourceInfo> getResourceTree() {
        List<AuthResource> list=this.selectList(new EntityWrapper<>());
        //遍历成树
        return this.getResourceToTree(list);
    }
    /**
     * @notes: 获得对应资源，并遍历成树
     * @Author: junyang.li
     * @Date: 17:31 2019/9/24
     * @param resourceIds :
     * @return: java.util.List<com.bee.platform.common.entity.ResourceDTO>
     */
    @Override
    public List<AuthResourceInfo> getResourceTree(List<Integer> resourceIds) {
        if(CollectionUtils.isEmpty(resourceIds)){
            return new ArrayList<>();
        }
        //通过资源id查询资源
        List<AuthResource> list=this.selectList(new EntityWrapper<AuthResource>()
                .where("deleted=0").andNew().in("id",resourceIds));
        return this.getResourceToTree(list);
    }

    /**
     * @notes: 查询当前用户可访问的菜单
     * @Author: junyang.li
     * @Date: 11:10 2019/9/26
     * @param roleId : 当前用户角色id
     * @param cloudMafType : 子应用标识
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    @Override
    public ResponseResult<List<AuthResourceInfo>> getUserResource(Integer roleId, String cloudMafType) {
        List<AuthResourceInfo> list=this.getResourceByRoleId(roleId,cloudMafType);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }
    /**
     * @notes: 解析所有的资源位置信息
     * @Author: junyang.li
     * @Date: 17:29 2019/9/26
     * @param position : 当前资源位置
     * @param allPosition : 容器
     * @return: void
     */
    private void analysisPosition(String position, Set<String> allPosition){
        //菜单的位置如: 3-2-1-2  ,通过横线分割
        allPosition.add(position);
        int lastLine=position.lastIndexOf(ConstantsUtil.LINE);
        while (lastLine!=-1){
            position=position.substring(0,lastLine);
            allPosition.add(position);
            lastLine=position.lastIndexOf(ConstantsUtil.LINE);
        }
    }
    /**
     * @notes: 将查询出来的菜单资源遍历成树
     * @Author: junyang.li
     * @Date: 17:00 2019/9/24
     * @param list : 查询的菜单资源
     * @return: java.util.List<com.bee.platform.common.entity.ResourceDTO>
     */
    private List<AuthResourceInfo> getResourceToTree(List<AuthResource> list) {
        List<AuthResourceInfo> resources=new ArrayList<>();
        //判空
        if(!CollectionUtils.isEmpty(list)){
            Map<Integer, AuthResourceInfo> map=new HashMap<>(16);
            //放入map中，键是资源id ，值是资源对象,并将对象转换为目标对象
            List<AuthResourceInfo> all=list.stream().map(obj->{
                AuthResourceInfo dto= BeanUtils.copyProperties(obj, AuthResourceInfo.class);
                map.put(obj.getId(),dto);
                return dto;
            }).collect(Collectors.toList());
            //只遍历一次，把所有的子菜单往对应的父菜单中加
            all.forEach(obj->{
                if(ConstantsUtil.ZERO == obj.getPid()){
                    resources.add(obj);
                }else {
                    AuthResourceInfo parent = map.get(obj.getPid());
                    if(CollectionUtils.isEmpty(parent.getChildren())){
                        parent.setChildren(new ArrayList<>());
                    }
                    parent.getChildren().add(obj);
                }
            });
        }
        return resources;
    }
}
