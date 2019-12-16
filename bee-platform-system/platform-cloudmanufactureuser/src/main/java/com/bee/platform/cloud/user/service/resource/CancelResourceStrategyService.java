package com.bee.platform.cloud.user.service.resource;

import com.bee.platform.cloud.user.config.annotation.ResourceTypeAnn;
import com.bee.platform.cloud.user.dto.FunctionDTO;
import com.bee.platform.cloud.user.dto.FunctionListDTO;
import com.bee.platform.cloud.user.dto.RoleDetailDTO;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.cloud.user.config.SpringBeanUtils;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.enums.Status;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @description: resource 相关的策略分发类
 * @author: junyang.li
 * @create: 2019-11-05 10:34
 **/
@Slf4j
@Component
public class CancelResourceStrategyService{

    @Autowired
    private AuthResourceService authResourceService;
    /**
     * 资源上下文
     */
    private static final Map<String,AbstractResourceStrategy>
            RESOURCE_CONTEXT = new HashMap<>(16);

    /**
     *  容器启动时获得每一个具体模块的实现，并且放入容器中
     */
    public void init() {
        Map<String, AbstractResourceStrategy> beanMap = SpringBeanUtils.getBeanMap(AbstractResourceStrategy.class);
        try {
            for (Map.Entry<String, AbstractResourceStrategy> entry : beanMap.entrySet()) {
                AbstractResourceStrategy strategy=entry.getValue();
                Object real = SpringBeanUtils.getTarget(strategy);
                ResourceTypeAnn annotation = real.getClass().getAnnotation(ResourceTypeAnn.class);
                if (annotation!=null) {
                    RESOURCE_CONTEXT.put(annotation.platform().getValue(),strategy);
                }
            }
        } catch (Exception e) {
            log.error("获取目标代理对象失败:{}", e);
        }
    }

    /**
     * @notes: 返回资源相关的上下文对象
     * @Author: junyang.li
     * @Date: 11:38 2019/11/5
     * @return: java.util.Map<java.lang.String,com.bee.platform.cloud.user.service.resource.AuthResourceServiceAbstract>
     */
    public Map<String,AbstractResourceStrategy> getResourceContext(){
        return RESOURCE_CONTEXT;
    }
    /**
     * @notes: 返回资源相关的上下文对象
     * @Author: junyang.li
     * @Date: 11:38 2019/11/5
     * @return: java.util.Map<java.lang.String,com.bee.platform.cloud.user.service.resource.AuthResourceServiceAbstract>
     */
    public AbstractResourceStrategy getResourceImpl(PlatformType type){
        return type==null?null:RESOURCE_CONTEXT.get(type.getValue());
    }

    /**
     * @notes: 返回g对应模块功能相关的上下文对象
     * @Author: junyang.li
     * @Date: 11:38 2019/11/5
     * @return: java.util.Map<java.lang.String,com.bee.platform.cloud.user.service.resource.AuthResourceServiceAbstract>
     */
    public  List<FunctionListDTO> getFunctionImpl(PlatformType type){
        AbstractResourceStrategy service=getResourceImpl(type);
        if(service==null){
            return null;
        }
        return service.getFunction();
    }

    /**
     * @notes: 校验所有的功能id
     * @Author: junyang.li
     * @Date: 15:07 2019/11/5
     * @param functionIds :
     * @return: java.util.List<java.lang.Integer>
     */
    public List<Integer> checkoutFunctionIds(List<Integer> functionIds){
        if(CollectionUtils.isEmpty(functionIds)){
            return null;
        }
        List<Integer> allFunctionIds=authResourceService.getAllFunctionIds();
        if(allFunctionIds.isEmpty()){
            return allFunctionIds;
        }
        return functionIds.stream()
                .filter(allFunctionIds::contains)
                .collect(Collectors.toList());
    }

    /**
     * @notes: 查询角色关联的功能
     * @Author: junyang.li
     * @Date: 15:30 2019/11/5
     * @param resourcesIds : 角色关联的资源id
     * @return: com.bee.platform.cloud.user.dto.RoleDetailDTO
     */
    public RoleDetailDTO getFunctionByRole(List<Integer> resourcesIds){
        //对象
        RoleDetailDTO dto=new RoleDetailDTO();
        //查询app端所有的功能
        List<FunctionListDTO> app=getFunctionImpl(PlatformType.CLOUD_MAF_APP);
        //查询web端所有的功能
        List<FunctionListDTO> web=getFunctionImpl(PlatformType.CLOUD_MAF_WEB);
        //查询bi端所有的功能
        List<FunctionListDTO> bi=getFunctionImpl(PlatformType.CLOUD_MAF_BI);
        //返回结果
        return dto.setApp(isSelection(resourcesIds,app))
                .setWeb(isSelection(resourcesIds,web))
                .setBi(isSelection(resourcesIds,bi));
    }
    /**
     * @notes: 判断功能是否被选中
     * @Author: junyang.li
     * @Date: 15:38 2019/11/5
     * @param resourcesIds : 角色对应的资源id
     * @param function : 待判断功能
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    private List<FunctionListDTO> isSelection(List<Integer> resourcesIds,List<FunctionListDTO> function){
        //判空
        if(!CollectionUtils.isEmpty(function)){
            //遍历app资源，看那些资源已被选择
            function.forEach(obj->{
                if(resourcesIds.contains(obj.getResourceId())){
                    obj.setSelection(Status.TRUE.getKey());
                }
            });
            return function;
        }
        return null;
    }

    /**
     * @notes: 功能列表查询
     * @Author: junyang.li
     * @Date: 16:40 2019/9/20
     * @return: com.bee.platform.cloud.user.dto.FunctionDTO
     */
    public  FunctionDTO selectFunction() {
        List<FunctionListDTO> app=this.getFunctionImpl(PlatformType.CLOUD_MAF_APP);
        List<FunctionListDTO> web=this.getFunctionImpl(PlatformType.CLOUD_MAF_WEB);
        List<FunctionListDTO> bi=this.getFunctionImpl(PlatformType.CLOUD_MAF_BI);
        return new FunctionDTO().setApp(app).setWeb(web).setBi(bi);
    }
}
