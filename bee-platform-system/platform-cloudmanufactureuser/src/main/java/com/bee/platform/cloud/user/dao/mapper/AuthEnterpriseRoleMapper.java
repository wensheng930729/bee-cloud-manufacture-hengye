package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.user.entity.AuthEnterpriseRole;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表 Mapper 接口
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthEnterpriseRoleMapper extends BaseMapper<AuthEnterpriseRole> {
    /**
     * @param list :
     * @notes: 批量插入企业角色的关联数据
     * @Author: junyang.li
     * @Date: 16:04 2019/5/24
     * @return: void
     */
    void insertAll(List<AuthEnterpriseRole> list);

}
