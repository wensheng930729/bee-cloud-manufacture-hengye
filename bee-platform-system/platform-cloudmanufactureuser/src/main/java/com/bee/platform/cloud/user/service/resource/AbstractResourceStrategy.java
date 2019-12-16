package com.bee.platform.cloud.user.service.resource;

import com.bee.platform.cloud.user.dto.FunctionListDTO;
import com.bee.platform.cloud.user.entity.AuthResource;

import java.util.List;

/**
 * @description: 将每个木块相同的方法抽象出来
 * @author: junyang.li
 * @create: 2019-11-05 16:11
 **/
public abstract class AbstractResourceStrategy {

    /**
     * @notes: 获得模块的所有功能的抽象
     * @Author: junyang.li
     * @Date: 9:39 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    public abstract List<FunctionListDTO> getFunction();
    /**
     * @notes: 获得模块的所有功能的抽象
     * @Author: junyang.li
     * @Date: 9:39 2019/11/5
     * @return: java.util.List<com.bee.platform.cloud.user.dto.FunctionListDTO>
     */
    public abstract List<AuthResource> getResource();
    /**
     * @notes: 校验功能id是否存在，并且返回存在的功能id 的抽象
     * @Author: junyang.li
     * @Date: 10:10 2019/11/5
     * @param functionIds :
     * @return: java.util.List<java.lang.Integer>
     */
    public abstract  List<Integer> checkFunctionIds(List<Integer> functionIds);
}
