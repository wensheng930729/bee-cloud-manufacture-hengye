package com.bee.platform.common.service;

/**
 * @ClassName GenerateIdService
 * @Description 序列号相关处理
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/28 14:01
 */
public interface GenerateIdService {

    /**
     * 获取工单编号
     * @return
     */
    String getWorkOrderNumber();

    /**
     * @Description 生成业务id
     * @author chenxm66777123
     * @Date 2018年12月24日
     * @version 1.0.0
     */
    public String generateBusinessId(Integer businessModuleId);

}
