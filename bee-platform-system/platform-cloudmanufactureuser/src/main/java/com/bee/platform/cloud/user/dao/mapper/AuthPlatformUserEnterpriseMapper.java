package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.user.entity.AuthPlatformUserEnterprise;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 企业与用户中间表 Mapper 接口
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthPlatformUserEnterpriseMapper extends BaseMapper<AuthPlatformUserEnterprise> {
    
    /**
     * @Description 批量添加用户管理企业
     * @Param null
     * @Date 2019/5/23 14:12
     * @Author xin.huang
     * @Return
     */
    int batchInsert(List<AuthPlatformUserEnterprise> list);
    /**
     * 查询用户所关联的企业id
     * @param userId
     * @return
     */
    List<Integer> findUserEnterpriseIds(@Param("userId") Integer userId);

    /**
     * 查询用户所关联的企业id
     * @param userId
     * @return
     */
    List<Integer> findUserByEnterpriseId(@Param("enterpriseId") Integer enterpriseId,
                                         @Param("data") List<Integer> userId);
}
