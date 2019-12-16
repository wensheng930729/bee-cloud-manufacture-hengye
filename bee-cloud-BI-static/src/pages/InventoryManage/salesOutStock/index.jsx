import { message, Divider } from 'antd';
import React, { useState, useEffect } from 'react';
import { connect } from 'dva';
import moment from 'moment';
import Link from 'umi/link';
import {
  FormItemInputSearch,
  FormItemRangePicker,
  FormItemInput,
  FormItemSelect,
  TableWrapper,
  CustomModalHOC,
  CustomFormHOC,
} from '@/components/FormWidget';

import {
  selectUnLoadContractCar,
  searchSaleOutOfStockList,
} from '../services/salesOutStockService';

import styles from './index.less';

const startTime = moment()
  .subtract(30, 'days')
  .format('YYYY-MM-DD');

const endTime = moment().format('YYYY-MM-DD');

// 顶部表单查询组件
const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
    isOutStock,
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="contractId"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按合同号查询' }}
      />
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="customerName"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按客户名称查询' }}
      />
      {isOutStock ? (
        <FormItemRangePicker
          getFieldDecorator={getFieldDecorator}
          label="出库日期"
          fieldId="dateRange"
          required={false}
          datePickerProps={{ format: 'YYYY-MM-DD' }}
          formItemLayout={formItemLayout}
        />
      ) : null}
    </>
  );
};

/*
新增出库的modal弹窗
*/
const ComfirmForm = props => {
  const {
    form: { getFieldDecorator },
    storageOptions,
    productsOptions,
    productSpecOptions,
  } = props;
  const formItemLayout = {
    labelCol: { span: 4 },
    wrapperCol: { span: 16 },
  };

  return (
    <>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="规格"
        fieldId="productSpecId"
        required
        selectProps={{ options: productSpecOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="仓库"
        fieldId="storageId"
        required
        selectProps={{ options: storageOptions }}
        formItemLayout={formItemLayout}
      />

      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label="领用数量"
        fieldId="productNumber"
        required
        formItemLayout={formItemLayout}
      />
      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label="领用人"
        fieldId="receiver"
        required
        formItemLayout={formItemLayout}
      />
    </>
  );
};
// 创建表单
const NewComfirmForm = CustomFormHOC(ComfirmForm);
// 创建modal
const NewComfirmFormModal = CustomModalHOC(NewComfirmForm);

/*
查看新增出库详情
*/
const CheckViewComfirm = props => {
  const { detailDataWithAdd } = props;
  console.log('detailDataWithAdd', detailDataWithAdd);
  return (
    <div>
      {detailDataWithAdd.map(item => (
        <div>
          <div style={{ marginLeft: '40%' }}>
            <p>
              <span className={styles.label}> 产品名称:</span>
              {item.productName}
            </p>
            <p>
              <span className={styles.label}> 产品规格:</span>
              {item.productSpecName}
            </p>
            <p>
              <span className={styles.label}> 仓库:</span>
              {item.storageName}
            </p>
            <p>
              <span className={styles.label}> 领用人:</span>
              {item.receiver}
            </p>
            <p>
              <span className={styles.label}> 领用数量:</span>
              {item.productNumber}
            </p>

            <p>
              <span className={styles.label}> 出库时间:</span>
              {item.receiveTime}
            </p>
          </div>
          <Divider />
        </div>
      ))}
    </div>
  );
};
// 创建modal
const CheckViewComfirmModal = CustomModalHOC(CheckViewComfirm);

const Index = props => {
  const {
    dispatch,
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  } = props;
  // 前端默认对 日期列降序排列
  const [sortOrder, setSortOrder] = useState('descend');
  // 当前TAB栏目
  const [tabKey, setTabKey] = useState('1');
  // "新增出库"弹窗是否可见
  const [confirmModalVisible, setConfirmModalVisible] = useState(false);

  // "查看新增出库"弹窗是否可见
  const [checkViewConfirmModalVisible, setCheckViewConfirmModalVisible] = useState(false);

  // 这里用于请求 顶部搜索表单中的下拉框数据
  useEffect(() => {
    // 这里在mount时执行一次
    dispatch({
      type: 'salesOutStockModel/fetchInitOptionsEffect',
      payload: {},
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }, []);

  const otherColumnsForWait = [];

  const otherColumns = [
    {
      title: '出库数量',
      dataIndex: 'number',
    },
    {
      title: '出库日期',
      dataIndex: 'modifyTime',
      sortOrder,
      sorter: (a, b) => {
        if (a.modifyTime && b.modifyTime) {
          const yearA = a.modifyTime.slice(0, 4);
          const monthA = a.modifyTime.slice(5, 7);
          const dayA = a.modifyTime.slice(8, 10);

          const newAStr = `${yearA}-${monthA}-${dayA}`;

          const yearB = b.modifyTime.slice(0, 4);
          const monthB = b.modifyTime.slice(5, 7);
          const dayB = b.modifyTime.slice(8, 10);

          const newBStr = `${yearB}-${monthB}-${dayB}`;

          const bool = moment(newAStr).isBefore(newBStr);

          return bool;
        }
        return false;
      },
    },
  ];

  const columns = [
    {
      title: '对应合同号',
      dataIndex: 'contractId',
    },
    {
      title: '客户',
      dataIndex: 'customerName',
    },
    {
      title: '承运商',
      dataIndex: 'carrierName',
    },

    {
      title: '车牌号',
      dataIndex: 'licensePlateNumber',
    },
    {
      title: '司机',
      dataIndex: 'driverName',
    },
    {
      title: '联系方式',
      dataIndex: 'contact',
    },

    {
      title: '操作',
      dataIndex: 'operation',

      render: (text, record) => (
        <Link
          to={{
            pathname: '/inventoryManage/salesOutStock/detail',
            query: { id: record.id, isOutStock: tabKey === '2' },
          }}
        >
          查看详情
        </Link>
      ),
    },
  ];

  if (tabKey === '1') {
    columns.splice(3, 0, ...otherColumnsForWait);
  } else {
    columns.splice(3, 0, ...otherColumns);
  }

  // 表格改变事件， 手动控制排序规则
  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };
  // tab栏目数据
  const parentTabs = [
    { value: '1', label: '待出库' },
    { value: '2', label: '已出库' },
  ];

  const commonTableProps = {
    parentTabs,
    // 顶部搜索表单中的属性，
    searchFormProps: {
      isOutStock: tabKey === '2',
    },
    // 设置表单默认值， 默认查询条件
    searchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,

      contractId: '',
      customerName: '',
      dateRange: {
        startTime,
        endTime,
      },
    },
    dispatch,
    // 表格改变事件
    handleTableChange,
    // tab栏目改变事件
    onTabChange: activeKey => setTabKey(activeKey),
  };

  const rowKey = 'id';

  // 当tabkey===2 时的  TableWrapper 组件props
  const CusTomTableProps = {
    ...commonTableProps,
    // 表格属性
    tableProps: {
      columns,
      rowKey,
    },
    operateText: '查看已新增出库信息',
    onOperateClick: () => {
      dispatch({
        type: 'salesOutStockModel/getFreeStorageEffect',
        payload: {},
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            setCheckViewConfirmModalVisible(true);
          }
        },
      });
    },
    listService: searchSaleOutOfStockList,
  };
  // 当tabkey===1 时的  TableWrapper 组件props
  const CusTomTablePropsForWait = {
    ...commonTableProps,
    tableProps: {
      columns,
      rowKey,
    },

    operateText: '新增出库',
    onOperateClick: () => setConfirmModalVisible(true),
    listService: selectUnLoadContractCar,
  };
  // 缓存 待入库- 新增出库的表单数据
  const cacheSaveFreeStorage = field => {
    dispatch({
      type: 'salesOutStockModel/cacheSaveFreeStorageEffect',
      payload: field,
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // "新增出库"modal弹窗props
  const ComfirmFormModalProps = {
    visible: confirmModalVisible,
    onOk: () => {
      try {
        _czc1.push(['_trackEvent', '销售出库', '新增', '', '', '']);
      } catch (error) {}
      dispatch({
        type: 'salesOutStockModel/saveFreeStorageEffect',
        payload: {},
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            message.success(msg);
            setConfirmModalVisible(false);
          }
        },
      });
    },
    onCancel: () => setConfirmModalVisible(false),
    title: '确认出库',
    ComProps: {
      storageOptions,
      productsOptions,
      productSpecOptions,
      dataDetail: saveFreeStorageParams,
      onChange: field => cacheSaveFreeStorage(field),
    },
  };

  const myProps = tabKey === '1' ? { ...CusTomTablePropsForWait } : { ...CusTomTableProps };

  const paramsHandler = params => {
    const { dateRange: newdateRange, contractId, customerName, ...other } = params;

    const requestParams = {
      startTime: newdateRange.startTime,
      endTime: newdateRange.endTime,
      contractId,
      customerName,

      page: {
        ...other,
      },
    };
    return requestParams;
  };

  // 查看确认新增出库的  详情
  const CheckViewComfirmModalProps = {
    visible: checkViewConfirmModalVisible,
    onOk: () => setCheckViewConfirmModalVisible(false),
    onCancel: () => setCheckViewConfirmModalVisible(false),
    title: '新增出库的详情',

    ComProps: {
      detailDataWithAdd,
    },
  };

  return (
    <>
      <TableWrapper {...myProps} currentTabKey={tabKey} paramsHandler={paramsHandler}>
        <MyFormSearch />
      </TableWrapper>
      <NewComfirmFormModal {...ComfirmFormModalProps} />
      <CheckViewComfirmModal {...CheckViewComfirmModalProps} />
    </>
  );
};

// 绑定redux
export default connect(state => {
  const {
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  } = state.salesOutStockModel;

  return {
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  };
})(Index);
