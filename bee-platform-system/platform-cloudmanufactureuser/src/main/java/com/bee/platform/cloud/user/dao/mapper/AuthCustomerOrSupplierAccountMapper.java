package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.AuthCustomerOrSupplierSearchDTO;
import com.bee.platform.cloud.user.entity.AuthCustomerOrSupplierAccount;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.user.rq.AuthCustomerAndSupplierSearchRQ;

import java.util.List;

/**
 * <p>
 * 客户账号和供应商账号 Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
public interface AuthCustomerOrSupplierAccountMapper extends BaseMapper<AuthCustomerOrSupplierAccount> {
    /**
     * 条件查询客户或供应商列表
     * @param rq 请求参数
     * @param pagination 分页对象
     * @return 客户或供应商列表
     */
    List<AuthCustomerOrSupplierSearchDTO> searchCustomerOrSupplierList(AuthCustomerAndSupplierSearchRQ rq, Pagination pagination);
}
