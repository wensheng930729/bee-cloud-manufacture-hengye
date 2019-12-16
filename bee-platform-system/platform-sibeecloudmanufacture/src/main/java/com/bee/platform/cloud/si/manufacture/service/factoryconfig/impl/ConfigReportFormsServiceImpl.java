package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigReportFormsMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsTreeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigReportForms;
import com.bee.platform.cloud.si.manufacture.rq.ConfigReportFormsUpdateStatusRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigReportFormsService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 报表配置表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@Service
public class ConfigReportFormsServiceImpl extends ServiceImpl<ConfigReportFormsMapper, ConfigReportForms> implements ConfigReportFormsService {

    /**
     * 条件查询报表配置列表
     *
     * @param userInfo 用户信息
     * @param name     名称
     * @param page     分页对象
     * @return 报表配置列表
     */
    @Override
    public ResponseResult<List<ConfigReportFormsDTO>> searchReportFormsList(AuthPlatformUserInfo userInfo, String name, Page page) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigReportForms> wrapper1 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("level", 3)
                .orderBy("id", true);
        if (!StringUtils.isEmpty(name)) {
            wrapper1.like("name", name);
        }
        // 查询所有报表配置
        List<ConfigReportForms> all = baseMapper.selectPage(pagination, wrapper1);
        List<String> allCodes = all.stream().map(o -> o.getCode()).collect(Collectors.toList());

        // 查询用户停用的报表配置
        Wrapper<ConfigReportForms> wrapper2 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.TRUE.getKey())
                .eq("level", 3)
                .in("code",allCodes)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigReportForms> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行替换处理
        if (!CollectionUtils.isEmpty(stop)) {

            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigReportForms> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());
            list.addAll(stop);

            List<ConfigReportFormsDTO> dto = BeanUtils.assemble(ConfigReportFormsDTO.class, list);

            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));


        }

        List<ConfigReportFormsDTO> dto = BeanUtils.assemble(ConfigReportFormsDTO.class, all);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据用户信息查询公司启用的报表列表
     *
     * @param userInfo 用户信息
     * @return 报表列表
     */
    @Override
    public List<ConfigReportFormsEnableDTO> getEnableReportFormsList(AuthPlatformUserInfo userInfo, String type) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();


        Integer pid;
        switch (type) {
            // 库存
            case "stock":
                pid = 11;
                break;
            // 质检
            case "quality_test":
                pid = 41;
                break;
            // 采购
            case "purchase":
                pid = 61;
                break;
            // 销售
            case "sale":
                pid = 81;
                break;
            // 产量分析
            case "throughput_analysis":
                pid = 101;
                break;
            // 合格率
            case "pass_rate":
                pid = 121;
                break;
            // 产量消耗分析
            case "output_and_consumption_analysis":
                pid = 141;
                break;
            // 物流
            case "logistics":
                pid = 161;
                break;
            // 匹配不上报错
            default:
                log.info("请求参数错误，没有找到相关匹配");
                throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.REPORT_FORMS_GET_FAILED);

        }

        Wrapper<ConfigReportForms> wrapper1 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.FALSE.getKey())
                .eq("pid", pid)
                .eq("level", 3)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", true);
        // 查询所有报表配置
        List<ConfigReportForms> all = baseMapper.selectList(wrapper1);

        // 查询用户停用的报表配置
        Wrapper<ConfigReportForms> wrapper2 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.TRUE.getKey())
                .eq("pid", pid)
                .eq("level", 3)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigReportForms> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行移除处理
        if (!CollectionUtils.isEmpty(stop)) {
            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigReportForms> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());

            return BeanUtils.assemble(ConfigReportFormsEnableDTO.class, list);

        }

        return BeanUtils.assemble(ConfigReportFormsEnableDTO.class, all);
    }

    /**
     * 停用报表
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Override
    public Integer stopReportForms(AuthPlatformUserInfo userInfo, ConfigReportFormsUpdateStatusRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer id = rq.getId();

        // 生成一条企业下停用的记录
        Wrapper<ConfigReportForms> wrapper1 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.FALSE.getKey())
                .eq("level", 3)
                .eq("deleted", Status.FALSE.getKey())
                .eq("id", id);
        ConfigReportForms exist = selectOne(wrapper1);
        if (ObjectUtils.isEmpty(exist)) {
            log.error("停用报表配置失败,没有查询到该id的数据,调用{}的{}方法出错", "ConfigReportFormsServiceImpl", "stopReportForms()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPORT_FORMS_UPDATE_FAILED_NO_DATA);
        }
        ConfigReportForms stop = BeanUtils.copyProperties(exist, ConfigReportForms.class);
        stop.setId(null)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setStatus(Status.FALSE.getKey())
                .setType(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setModifier(userName)
                .setModifyId(userId)
                .setModifyTime(time);

        // 保存停用报表配置
        if (!insert(stop)) {
            log.error("保存停用报表配置失败，调用{}的{}方法出错", "ConfigReportFormsServiceImpl", "stopReportForms()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPORT_FORMS_STOP_FAILED);

        }

        return id;
    }

    /**
     * 启用报表
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Override
    public Integer enableReportForms(AuthPlatformUserInfo userInfo, ConfigReportFormsUpdateStatusRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer id = rq.getId();


        // 查询用户停用的报表配置
        Wrapper<ConfigReportForms> wrapper = new EntityWrapper<ConfigReportForms>()
                .eq("id", id)
                .eq("level", 3)
                .eq("type", Status.TRUE.getKey())
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);
        ConfigReportForms stop = selectOne(wrapper);
        if (ObjectUtils.isEmpty(stop)) {
            log.error("启用报表配置失败,没有查询到该id的数据,调用{}的{}方法出错", "ConfigReportFormsServiceImpl", "enableReportForms()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPORT_FORMS_UPDATE_FAILED_NO_DATA);
        }
        stop.setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);

        // 启用看板BI配置
        if (!updateById(stop)) {
            log.error("启用报表配置失败，调用{}的{}方法出错", "ConfigReportFormsServiceImpl", "enableReportForms()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPORT_FORMS_ENABLE_FAILED);
        }

        return id;
    }


    /**
     * 查询报表完整树
     *
     * @param userInfo 用户信息
     * @return 报表树
     */
    @Override
    public List<ConfigReportFormsTreeDTO> getReportFormsTree(AuthPlatformUserInfo userInfo) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Wrapper<ConfigReportForms> wrapper1 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", true);

        // 查询所有报表配置
        List<ConfigReportForms> all = baseMapper.selectList(wrapper1);

        // 查询用户停用的报表配置
        Wrapper<ConfigReportForms> wrapper2 = new EntityWrapper<ConfigReportForms>()
                .eq("type", Status.TRUE.getKey())
                .eq("level", 3)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigReportForms> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行替换处理
        if (!CollectionUtils.isEmpty(stop)) {

            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigReportForms> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());
            list.addAll(stop);

            List<ConfigReportFormsTreeDTO> dto = BeanUtils.assemble(ConfigReportFormsTreeDTO.class, list);

            return  buildByRecursive(dto);
        }

        List<ConfigReportFormsTreeDTO> dto = BeanUtils.assemble(ConfigReportFormsTreeDTO.class, all);

        return  buildByRecursive(dto);
    }


    /**
     * 使用递归方法建树
     *
     * @param treeNodes
     * @return
     */
    public static List<ConfigReportFormsTreeDTO> buildByRecursive(List<ConfigReportFormsTreeDTO> treeNodes) {
        List<ConfigReportFormsTreeDTO> trees = new ArrayList<>();
        for (ConfigReportFormsTreeDTO treeNode : treeNodes) {
            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == treeNode.getPid()) {
                trees.add(findChildren(treeNode, treeNodes));
            }
        }
        return trees;
    }

    /**
     * 递归查找子节点
     *
     * @param treeNodes
     * @return
     */
    public static ConfigReportFormsTreeDTO findChildren(ConfigReportFormsTreeDTO treeNode, List<ConfigReportFormsTreeDTO> treeNodes) {
        for (ConfigReportFormsTreeDTO it : treeNodes) {
            if (treeNode.getId().equals(it.getPid())) {
                if (treeNode.getChildren() == null) {
                    treeNode.setChildren(new ArrayList<>());
                }
                treeNode.getChildren().add(findChildren(it, treeNodes));
            }
        }
        return treeNode;
    }

}