package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.user.entity.AuthInterface;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
public interface AuthInterfaceMapper extends BaseMapper<AuthInterface> {

    /**
     * @notes: 批量插入接口
     * @Author: junyang.li
     * @Date: 15:05 2019/5/28
     * @param list :
     * @return: void
     */
    void insertAll(List<AuthInterface> list);
}
