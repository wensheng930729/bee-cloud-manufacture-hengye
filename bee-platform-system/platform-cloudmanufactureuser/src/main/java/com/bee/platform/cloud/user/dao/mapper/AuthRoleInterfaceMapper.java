package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.user.entity.AuthRoleInterface;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
public interface AuthRoleInterfaceMapper extends BaseMapper<AuthRoleInterface> {

    /**
     * @Description 批量添加接口角色
     * @Param authRoleInterfaces
     * @Date 2019/5/21 14:52
     * @Author xin.huang
     * @Return
     */
    public int batchInsert(List<AuthRoleInterface> authRoleInterfaces);
}
