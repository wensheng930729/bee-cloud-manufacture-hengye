import React, { useState } from 'react';
import { message } from 'antd';
import moment from 'moment';
import { connect } from 'dva';

import { FormItemInputSearch, FormItemRangePicker, TableWrapper } from '@/components/FormWidget';
import ContractRelated from './contractRelated';
import { getWeightMachineWebBindList } from './services';
import styles from './index.less';
// 顶部表单查询组件
const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="custOrSupName"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按客户/供应商名称搜索' }}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="过磅日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

// table的refs
let tableRef = {};
const Index = props => {
  const { dispatch } = props;
  // 前端默认对 日期列降序排列
  const [sortOrder, setSortOrder] = useState('descend');
  // 当前TAB栏目
  const [tabKey, setTabKey] = useState('1');
  // 弹窗是否可见
  const [confirmModalVisible, setConfirmModalVisible] = useState(false);
  // 弹框提交选择的行数据
  const [selectRow, setSelectRow] = useState([]);
  // table选择的行数据
  const [selectTableRows, setSelectTableRows] = useState([]);

  const [selectedSelf, setSelectedSelf] = useState([]);

  const [alertMessageNum, setAlertMessageNum] = useState(0);

  // 最外层 table行选择数据
  // 将选择得到的数据进行处理(id,类型,产品名,供货商名，总重量)
  const handleSelectRows = (record, selected) => {
    if (selected) {
      setSelectedSelf(selected);
    }
    let weightNum = 0;
    const datas = selected.map(item => {
      const selectTableRowsObj = {};
      selectTableRowsObj.machineId = item.machineId;
      selectTableRowsObj.type = item.type;
      weightNum += item.netWeight;
      selectTableRowsObj.productName = item.productName;
      selectTableRowsObj.custOrSupName = item.custOrSupName;
      return selectTableRowsObj;
    });
    if (selected.length > 0) {
      setAlertMessageNum(weightNum.toFixed(3));
    } else {
      setAlertMessageNum(0);
    }
    setSelectTableRows(datas);
  };
  // 忽略/恢复
  const handleIgnore = obj => {
    const {
      // 忽略还是恢复
      isIgnore,
      // type 磅单类型 1 采购 2 销售
      record: { machineId, type },
    } = obj;
    const payloadSelf = { machineId, type, isIgnore };
    dispatch({
      type: 'contractRelated/ignoreRecoveryMachine',
      payload: payloadSelf,
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          tableRef.setInitialValue();
          message.success(msg);
        }
        setSelectTableRows([]);
        setSelectedSelf([]);
        setAlertMessageNum(0);
      },
    });
  };
  // 列columns
  const columns = [
    {
      title: '磅单号',
      dataIndex: 'machineId',
    },
    {
      title: '客户/供应商',
      dataIndex: 'custOrSupName',
    },
    {
      title: '承运商',
      dataIndex: 'carrierName',
    },
    {
      title: '车号',
      dataIndex: 'trainNumber',
    },
    {
      title: '产品名称',
      dataIndex: 'productName',
    },
    {
      title: '净重',
      dataIndex: 'netWeight',
    },
    {
      title: '司磅员',
      dataIndex: 'weightMan',
    },
    {
      title: '司磅日期',
      dataIndex: 'weighingTime',
      sortOrder,
      sorter: (a, b) => {
        const yearA = a.weighingTime.slice(0, 4);
        const monthA = a.weighingTime.slice(5, 7);
        const dayA = a.weighingTime.slice(8, 10);

        const newAStr = `${yearA}-${monthA}-${dayA}`;

        const yearB = b.weighingTime.slice(0, 4);
        const monthB = b.weighingTime.slice(5, 7);
        const dayB = b.weighingTime.slice(8, 10);

        const newBStr = `${yearB}-${monthB}-${dayB}`;

        const bool = moment(newAStr).isBefore(newBStr);

        return bool;
      },
      ellipsis: true,
    },
    {
      title: '操作',
      dataIndex: 'operation',
      key: 'operation',
      render: (text, record) => (
        <span>
          {tabKey === '1' && (
            <>
              {/* <a
                onClick={() => {
                  handleModalFun(record, index);
                }}
              >
                关联合同
              </a>
              <Divider type="vertical" /> */}
              {/* 点击时isIgnore为false，显示忽略按钮 */}
              <a onClick={() => handleIgnore({ isIgnore: 0, record })}>忽略</a>
            </>
          )}
          {/* tab选择已忽略时,isIgnore为true,显示恢复按钮 */}
          {tabKey === '0' && <a onClick={() => handleIgnore({ isIgnore: 1, record })}>恢复</a>}
        </span>
      ),
    },
  ];

  // 表格改变事件， 手动控制排序规则
  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };
  // tab栏目数据
  const parentTabs = [
    { value: '1', label: '待关联' },
    { value: '0', label: '已忽略' },
  ];
  // tab栏目改变事件
  const onTabChange = activeKey => {
    setSelectTableRows([]);
    setSelectRow([]);
    setTabKey(activeKey);
  };
  const commonTableProps = {
    // tab栏目数据
    parentTabs,
    // 设置表单默认值， 默认查询条件
    searchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,
      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },

      isIgnore: tabKey,
      custOrSupName: '',
    },
    // 表格改变事件
    handleTableChange,
    // tab栏目改变事件
    onTabChange,
  };

  // 当tabkey===0 时的  TableWrapper 组件props
  const CusTomTableProps = {
    ...commonTableProps,
    // 表格属性
    tableProps: {
      hasRowSelection: false,
      columns,
      selectedRows: selectTableRows,
    },
  };
  // 当tabkey===1 时的  TableWrapper 组件props
  const CusTomTablePropsForWait = {
    ...commonTableProps,
    tableProps: {
      hasRowSelection: true,
      columns,
      selectedRows: selectTableRows,
      rowSelection: {
        getCheckboxProps: record => ({
          disabled:
            selectedSelf.length === 0
              ? false
              : !(
                  record.custOrSupName === selectedSelf[0].custOrSupName &&
                  record.productName === selectedSelf[0].productName
                ), // Column configuration not to be checked
          name: record.custOrSupName,
        }),
      },
    },
    operateText: '关联合同',
    onOperateClick: () => {
      if (selectTableRows.length) {
        setConfirmModalVisible(true);
      } else {
        message.error('请选择');
      }
    },
  };

  const alertProps = {
    hasAlert: true,
    alertLayout: (
      <span>
        总计{' '}
        <a
          style={{
            fontWeight: 600,
          }}
        >
          {alertMessageNum}
        </a>{' '}
        吨&nbsp;&nbsp;
      </span>
    ),
  };

  // 本方法会将TableWrapper中所需要的属性进行赋值
  //
  const paramsHandler = params => {

    // 需要在每次请求数据之前初始化选择的项
    setSelectTableRows([]);
    setSelectRow([]);
    setSelectedSelf([]);
    setAlertMessageNum(0);

    const {
      dateRange: newdateRange,
      isIgnore,
      contractNum,
      supplierName,
      custOrSupName,
      ...other
    } = params;

    const requestParams = {
      ...{
        startTime: newdateRange.startTime,
        endTime: newdateRange.endTime,
        isIgnore,
        custOrSupName,
      },
      ...other,
    };
    return requestParams;
  };

  // 弹窗里面的单选框选中事件
  const onSelectRow = (selectedRowKeys, selectedRows) => {
    setSelectRow(selectedRows);
  };

  //  点击 合同或者忽略按钮  关联 modal弹窗props
  const ComfirmFormModalProps = {
    width: '80%',
    visible: confirmModalVisible,
    onOk: () => {
      if (selectRow.length === 0) {
        message.warning('请选择关联合同');
        return;
      }
      const datas = selectTableRows.map(item => ({
        ...item,
        contractBusinessId: selectRow[0].contractBusinessId,
        contractNum: selectRow[0].contractNum,
      }));
      dispatch({
        type: 'contractRelated/machineBindContract',
        payload: datas,
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            setSelectTableRows([]);
            setSelectRow([]);
            tableRef.setInitialValue();
            message.success(msg);
            setConfirmModalVisible(false);
            setSelectedSelf([]);
          }
        },
      });

      // if (selectTableRows.length) {
      //   setConfirmModalVisible(true);
      //   // 关联合同;
      //   handleOnOkAll('contractRelated/machineBindContract', selectTableRows, '1');
      // } else {
      //   message.error('请选择');
      // }
    },
    onCancel: () => {
      setConfirmModalVisible(false);
      setSelectRow([]);
    },
    title: tabKey === '1' ? '关联合同' : '恢复',
    ComProps: {
      onSelectRow,
      productName: selectTableRows && selectTableRows.length ? selectTableRows[0].productName : '',
      custOrSupName:
        selectTableRows && selectTableRows.length ? selectTableRows[0].custOrSupName : '',
    },
  };

  const myProps = tabKey === '1' ? { ...CusTomTablePropsForWait } : { ...CusTomTableProps };

  return (
    <div className={styles.contractStatement}>
      {/* Table包装类 */}
      <TableWrapper
        {...myProps}
        tableWrapperRef={a => {
          tableRef = a;
        }}
        listService={getWeightMachineWebBindList}
        // 用来设置当前的Tab页面
        currentTabKey={tabKey}
        // 本方法用来筛选需要映射的对象
        paramsHandler={paramsHandler}
        // 当checkbox被选择时就会触发
        handleSelectRows={handleSelectRows}
        // 包含是否能够显示alertProps的判断以及alertProp显示组件
        alertProps={alertProps}
      >
        <MyFormSearch />
      </TableWrapper>
      <ContractRelated {...ComfirmFormModalProps} />
    </div>
  );
};

export default connect()(Index);
